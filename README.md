# A Tool for Verifying Programs using Constraint Horn Clauses

Description!?

## Installation

Install [Java](https://www.java.com/de/download/manual.jsp)
```bash
sudo apt-get update
sudo apt-get install default-jdk
```
Install [Python3](https://www.python.org/download/releases/3.0/)
```bash
sudo apt-get install python3 
```

Install [Scala 2.11.12](https://scala-lang.org/download/2.11.12.html)
```bash
sudo apt-get install scala
```

Use the package manager [pip](https://pip.pypa.io/en/stable/) to install [Z3](https://github.com/Z3Prover/z3).

```bash
sudo apt install python3-pip
pip install z3-solver
```

## Usage
Inside the src folder where the toHorn.scala file lies run:
```bash
scala -nc toHorn.scala ../FILEPATH/inputFile.txt [-bv BitVectorSize]
```
The optional parameter -bv BitvectorSize determines if bitvector arithmetic is to be used instead of integer arithmetic
