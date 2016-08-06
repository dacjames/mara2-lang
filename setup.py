from setuptools import setup

setup(
    name='mara-llvm',
    version='1.2.0',
    description='Mara LLVM Service',
    license='MIT',
    packages=['src/main/py/mara_llvm'],
    install_requires=['llvmlite'],
)