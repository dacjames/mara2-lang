<template>
  <div id="app">
    <img src="./assets/PinkSquareLogo.jpg">
    <h1>{{ title }}</h1>
    <h2>{{ subtitle }}</h2>
    <pre>
      <code>
      def fib(x: Int) -> Int {
        if x <= 1 { 1 }
        else { self(x - 2) + self(x - 1) }
      }

      app main(args: List(String)) {
        fib(10).show
      }
      </code>
    </pre>
  </div>
</template>

<script lang="ts">
export default {
  name: 'app',
  data(): Object {
    return {
      title: 'Mara',
      subtitle: 'A fun new language',
    }
  }
}
</script>

<style lang="scss">
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}

h1, h2 {
  font-weight: normal;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  display: inline-block;
  margin: 0 10px;
}

a {
  color: #42b983;
}

pre {
  text-align: left;
  margin: auto;
  width: 50%;
  border: 1px solid black;
  border-radius: 5px;
}

</style>
