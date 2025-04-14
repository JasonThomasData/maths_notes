# Three-Species Example: Press Perturbation Response

We'll work through a concrete example using a system of 3 species. Suppose our system is described by:

$$
\frac{dn_1}{dt} = f_1(n_1, n_2, n_3), \quad
\frac{dn_2}{dt} = f_2(n_1, n_2, n_3), \quad
\frac{dn_3}{dt} = f_3(n_1, n_2, n_3).
$$

At the original equilibrium,

$$
\mathbf{n}^* = (n_1^*, n_2^*, n_3^*),
$$

we have

$$
f_1(n_1^*, n_2^*, n_3^*) = 0, \quad
f_2(n_1^*, n_2^*, n_3^*) = 0, \quad
f_3(n_1^*, n_2^*, n_3^*) = 0.
$$

---

## Applying a Press Perturbation

Now, suppose we apply a small, sustained perturbation $P_1$ to species 1. The modified system becomes:

$$
\frac{dn_1}{dt} = f_1(n_1, n_2, n_3) + P_1,
$$

$$
\frac{dn_2}{dt} = f_2(n_1, n_2, n_3),
$$

$$
\frac{dn_3}{dt} = f_3(n_1, n_2, n_3).
$$

At the new equilibrium, denote the abundances by

$$
\mathbf{n}^e = (n_1^e, n_2^e, n_3^e).
$$

Thus, the equilibrium conditions are:

$$
0 = f_1(n_1^e, n_2^e, n_3^e) + P_1,
$$

$$
0 = f_2(n_1^e, n_2^e, n_3^e),
$$

$$
0 = f_3(n_1^e, n_2^e, n_3^e).
$$

---

## Differentiation with Respect to $P_1$

We now wish to know how the new equilibrium shifts with respect to $P_1$, i.e. we want to compute $\frac{dn_i^e}{dP_1}$ for each species.

Differentiate the equilibrium conditions with respect to $P_1$:

- **For species 1:**

$$
\frac{d}{dP_1}\Bigl[f_1(n_1^e, n_2^e, n_3^e) + P_1\Bigr] = 0 
\quad \Longrightarrow \quad 
A_{11}\frac{dn_1^e}{dP_1} + A_{12}\frac{dn_2^e}{dP_1} + A_{13}\frac{dn_3^e}{dP_1} + 1 = 0.
$$

- **For species 2:**

$$
\frac{d}{dP_1}f_2(n_1^e, n_2^e, n_3^e) = 0 
\quad \Longrightarrow \quad 
A_{21}\frac{dn_1^e}{dP_1} + A_{22}\frac{dn_2^e}{dP_1} + A_{23}\frac{dn_3^e}{dP_1} = 0.
$$

- **For species 3:**

$$
\frac{d}{dP_1}f_3(n_1^e, n_2^e, n_3^e) = 0 
\quad \Longrightarrow \quad 
A_{31}\frac{dn_1^e}{dP_1} + A_{32}\frac{dn_2^e}{dP_1} + A_{33}\frac{dn_3^e}{dP_1} = 0.
$$

Here, we define the Jacobian matrix $A$ by

$$
A_{ik} = \frac{\partial f_i}{\partial n_k},
$$

evaluated at the equilibrium.

---

## Matrix Formulation

We can express the three equations compactly using the Kronecker delta $\delta_{i1}$ (where $\delta_{i1} = 1$ if $i=1$ and $0$ otherwise):

$$
\sum_{k=1}^3 A_{ik}\frac{dn_k^e}{dP_1} = -\delta_{i1}, \quad \text{for } i = 1, 2, 3.
$$

Define the vector of responses:

$$
\frac{d\mathbf{n}^e}{dP_1} =
\begin{bmatrix}
\frac{dn_1^e}{dP_1} \\
\frac{dn_2^e}{dP_1} \\
\frac{dn_3^e}{dP_1}
\end{bmatrix},
$$

and the unit vector

$$
\mathbf{e}_1 =
\begin{bmatrix}
1 \\
0 \\
0
\end{bmatrix}.
$$

Then, the system can be written in matrix form as:

$$
A \frac{d\mathbf{n}^e}{dP_1} = -\mathbf{e}_1,
$$

where

$$
A =
\begin{bmatrix}
A_{11} & A_{12} & A_{13} \\
A_{21} & A_{22} & A_{23} \\
A_{31} & A_{32} & A_{33}
\end{bmatrix}.
$$

Assuming $A$ is invertible (which is justified by the local stability of the original equilibrium), the solution is:

$$
\frac{d\mathbf{n}^e}{dP_1} = -A^{-1}\mathbf{e}_1.
$$

This result tells us how each species' equilibrium abundance changes in response to a small, sustained press perturbation on species 1. For perturbations applied to species 2 or 3, use the corresponding unit vector (e.g., $\mathbf{e}_2 = \begin{bmatrix} 0 \\ 1 \\ 0 \end{bmatrix}$).

---

[Back ➡️](slide3_1.md)