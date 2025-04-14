# Saving Results: File Format Guide

You'll save **two CSV files** for your ecosystem.

---

## 📁 File 1: `yourlastname_ecosystem_parameters.csv`

This stores the **model parameters** for $N$ ecosystem instances with stable, feasible equilibria.

- **Rows = 2S + S²**  
- **Columns = N**

**Row layout**:
1. First $S$ rows: growth rates $r_1, r_2, ..., r_S$
2. Next $S$ rows: equilibrium abundances $x_1^*, ..., x_S^*$
3. Next $S^2$ rows: interaction matrix $A$ flattened column-wise

---

## 📁 File 2: `yourlastname_ecosystem_predictions.csv`

This stores the **comparison data**.

- **Rows = 2 × S²**
- **Columns = N** (same order as in first file)

**Row layout**:
1. First $S^2$ rows: vectorised version of the signed **press perturbation** response matrix
2. Second $S^2$ rows: vectorised **eradication** response matrix

---

This format lets us:
- Directly compare local (press) and global (removal) responses
- Quantify how different they are — and when that matters
- Reconstruct all ecosystems and predictions later for analysis and plotting

[⬅️ Back](slide2_3.md) | [Next ➡️](slide2_5.md)

## Side note: What Does "Vectorising a Matrix" Mean?

When we say **vectorise** a matrix, we mean:  
👉 Turn it into a **column vector** by stacking its columns on top of each other.

### Example

Suppose we have a $2 \times 2$ matrix:

$$
A = \begin{bmatrix}
a_{11} & a_{12} \\\\
a_{21} & a_{22}
\end{bmatrix}
$$

The **vectorised version** of $A$, written as $\text{vec}(A)$, is:

$$
\text{vec}(A) = \begin{bmatrix}
a_{11} \\\\
a_{21} \\\\
a_{12} \\\\
a_{22}
\end{bmatrix}
$$

We go **column by column**, stacking them from left to right.

This is the format you should use when saving both:
- The **interaction matrix $A$** in `ecosystem_parameters.csv`
- The **press and eradication matrices** in `ecosystem_predictions.csv`
