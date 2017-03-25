import asyncio
import json
import sys
import traceback

import msgpack


class InvalidMagicByte(Exception):
    pass


class MissingHeader(Exception):
    pass


class InvalidFormat(Exception):
    pass


async def _read_header(client_reader):
    header = await client_reader.read(2)
    if len(header) < 2:
        raise MissingHeader()
    elif header[:1].decode('utf-8') != 'D':
        raise InvalidMagicByte()
    else:
        sigil = header[1:2].decode('utf-8')
        return sigil


async def _read_json(client_reader):
    data = await client_reader.readline()
    print("Raw:", data)
    return json.loads(data.decode('utf-8'))


async def _read_msgpack(client_reader):
    data = await client_reader.readline()
    print("Raw:", data)
    return msgpack.unpackb(data.strip(), encoding='utf-8')


async def _read_message(client_reader):
    sigil = await _read_header(client_reader)
    if sigil == '0':
        message = await _read_json(client_reader)
    elif sigil == '1':
        message = await _read_msgpack(client_reader)
    else:
        raise InvalidFormat()
    return message


def _write_json(writer, message):
    writer.write('D'.encode('utf-8'))
    writer.write('0'.encode('utf-8'))
    writer.write(json.dumps(message).encode('utf-8'))
    writer.write(b'\n')
    return message


def _write_msgpack(writer, message):
    writer.write('D'.encode('utf-8'))
    writer.write('1'.encode('utf-8'))
    writer.write(msgpack.packb(message, use_bin_type=True))
    writer.write(b'\n')
    return message


class PortServer:
    def __init__(self, host='127.0.0.1', port=12345):
        self._host = host
        self._port = port

        self._loop = None
        self._server = None
        self._clients = {}

    def _accept_client(self, client_reader, client_writer):
        """
        Accept a new client connection and create a Task
        to handle this client.
        :param client_reader:
        :param client_writer:
        :return:
        """
        task = asyncio.Task(self._handle_client(client_reader, client_writer))
        self._clients[task] = (client_reader, client_writer)
        print("Added client task: ", task, file=sys.stderr)

        @task.add_done_callback
        def client_done(task):
            print("Client task done: ", task, file=sys.stderr)
            del self._clients[task]
            task.cancel()

    async def _handle_client(self, client_reader, client_writer):
        """
        Handle requests from the client.
        :param client_reader: StreamReader
        :param client_writer: StreamWriter
        :return:
        """
        def respond(text):
            print('Responding', text)
            _write_json(client_writer, text)

        while True:
            try:
                message = await _read_message(client_reader)
                print("Message", message)
                respond({"message": message})

            except InvalidMagicByte:
                respond('Bad Magic')

            except InvalidFormat:
                respond('Invalid Format')

            except MissingHeader:
                client_writer.close()
                break

            except Exception:
                print("ERROR:", file=sys.stderr)
                traceback.print_exc(file=sys.stderr)
                respond('Unknown Error')

            await client_writer.drain()

    def start(self, loop):
        self._loop = loop
        self._server = loop.run_until_complete(
            asyncio.streams.start_server(self._accept_client, self._host, self._port, loop=loop)
        )
        print('Started')

    def stop(self):
        for task in self._clients:
            task.cancel()

        if self._server is not None:
            self._server.close()
            self._server = None
            self._loop = None

    async def wait(self):
        print('Waiting')
        await self._server.wait_closed()


class PortClient:
    def __init__(self, host='127.0.0.1', port=12345, encoding='json'):
        self._host = host
        self._port = port

        self._reader = None
        self._writer = None

        if encoding == 'json':
            self._write_message = _write_json
        elif encoding == 'msgpack':
            self._write_message = _write_msgpack
        else:
            raise ValueError(encoding)

    def start(self, loop):
        (self._reader, self._writer) = loop.run_until_complete(
            asyncio.streams.open_connection(self._host, self._port, loop=loop)
        )
        print('started client...')

    async def recv(self):
        message = await _read_message(self._reader)
        return message

    async def send(self, message):
        self._write_message(self._writer, message)
        await self._writer.drain()

    async def go(self):
        await self.send({"hello": "world"})
        response = await self.recv()
        print("Response", response)

        await self.send({"hello": "peeps"})
        response = await self.recv()
        print("Response", response)


    def stop(self):
        if self._reader is not None:
            self._reader = None
        if self._writer is not None:
            self._writer.close()
            self._writer = None


def main():
    loop = asyncio.get_event_loop()

    server = PortServer()
    server.start(loop)

    client = PortClient(encoding='msgpack')
    client.start(loop)

    loop.run_until_complete(client.go())
    loop.run_until_complete(
        server.wait(),
    )

    loop.close()
