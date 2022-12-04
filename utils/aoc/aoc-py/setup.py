from setuptools import setup

# with open('requirements.txt') as f:
#     install_requires = f.read().splitlines()

setup(
    name="aoc",
    # packages=['someprogram'],
    version="0.1.0",
    # author='...',
    # description='...',
    #  install_requires=install_requires,
    # scripts=[
    #   'aoc.py',
    # ],
    entry_points={
        # example: file some_module.py -> function main
        "console_scripts": ["aoc=aoc:main"]
    },
)
