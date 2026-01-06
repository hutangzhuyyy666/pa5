# COOL Compiler - Code Generator (PA5)

![Language](https://img.shields.io/badge/language-C%2B%2B-blue.svg)
![Architecture](https://img.shields.io/badge/arch-MIPS-red.svg)
![Status](https://img.shields.io/badge/build-passing-brightgreen.svg)

## 📖 Project Overview / 项目简介

**[English]**
This project implements the backend **Code Generator** for the COOL (Classroom Object-Oriented Language) compiler. It translates the static Abstract Syntax Tree (AST) into **MIPS assembly code**, which is executed on the SPIM simulator.

This implementation features a robust runtime environment design, correctly handling complex object-oriented features such as inheritance, polymorphism (dynamic dispatch), runtime type identification (case analysis), and deep recursion.

**[中文]**
本项目实现了 COOL 语言编译器的后端——**代码生成器**。它负责将经过语义分析的抽象语法树（AST）转换为可以在 SPIM 模拟器上运行的 **MIPS 汇编代码**。

本项目代码具有极高的鲁棒性，能够正确处理复杂的面向对象特性，如继承、多态（动态分发）、运行时类型识别（Case 语句）以及深层递归调用（栈帧管理）。

---

## 🚀 Key Features / 核心特性

*   **Dynamic Stack Management (`ScopeTracker`)**:
    *   Implemented a `ScopeTracker` class to calculate variable offsets dynamically relative to `$sp`.
    *   Solves the "stack pointer movement" issue during nested expression evaluations, ensuring correct variable access.
*   **Robust Object-Oriented Implementation**:
    *   **Dispatch Tables**: Correctly constructs VTables for inherited and overridden methods.
    *   **Polymorphism**: Implements dynamic dispatch logic compliant with COOL specifications.
    *   **Type Case Sorting**: Optimized tag-based checking for `case` expressions, correctly handling inheritance depth (e.g., distinguishing subclass `Poodle` from superclass `Dog`).
*   **Memory Management**:
    *   Fully compatible with the COOL Garbage Collector (GenGC) interface.

---

## 📂 Project Structure / 文件说明

*   **`cgen.cc`**: The core implementation. Handles the traversal of the AST and emits MIPS assembly instructions. Implements the `CgenClassTable` logic.
*   **`cgen.h`**: Header definitions for the code generator class table and nodes.
*   **`cool-tree.handcode.h`**: **[Critical]** Contains the definition of the `ScopeTracker` class and extensions to AST nodes to support code generation context.

---

## 🛠️ How to Compile / 如何编译

In the project directory (Linux environment):

```bash
# Clean previous builds
make clean

# Compile the code generator
make cgen