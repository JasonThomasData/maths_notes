# General Derivation of the Press Perturbation Response

We'll derive how the equilibrium abundances in an ecosystem shift when one species is subjected to a sustained press perturbation. This is relevant in conservation scenarios—such as continuous harvesting or management of an invasive species—where we want to predict how the entire ecosystem responds to a persistent change.

---

## Set-Up

Consider a general dynamical system with $S$ species:

$$
\frac{dn_i}{dt} = f_i(\mathbf{n}), \quad i = 1, 2, \dots, S,
$$

where:
- $\mathbf{n} = (n_1, n_2, \dots, n_S)$ is the vector of species abundances.
- $f_i(\mathbf{n})$ represents the net rate of change of species $i$.
- The indices $i$, $j$, and $k$ denote different species.

At the original equilibrium (before any perturbation), denoted by

$$
\mathbf{n}^* = (n_1^*, n_2^*, \dots, n_S^*),
$$

we have

$$
f_i(\mathbf{n}^*) = 0 \quad \text{for all } i.
$$

This equilibrium represents the balanced state of the ecosystem prior to external intervention.

---

## Press Perturbation

Now, suppose we apply a small, sustained perturbation $P_j$ to species $j$. The system then becomes:

$$
\frac{dn_j}{dt} = f_j(\mathbf{n}) + P_j,
$$

$$
\frac{dn_i}{dt} = f_i(\mathbf{n}) \quad \text{for } i \neq j.
$$

At the new equilibrium, denote the abundances by

$$
\mathbf{n}^e = (n_1^e, n_2^e, \dots, n_S^e).
$$

Thus, the new equilibrium conditions are:

$$
0 = f_j(\mathbf{n}^e) + P_j,
$$

$$
0 = f_i(\mathbf{n}^e) \quad \text{for } i \neq j.
$$

---

## Differentiation with Respect to $P_j$

We wish to determine how the new equilibrium shifts as we vary $P_j$, i.e. compute $\frac{dn_i^e}{dP_j}$. Differentiating the equilibrium conditions with respect to $P_j$, we obtain:

- **For species $j$:**

$$
\frac{d}{dP_j}\Bigl[f_j(\mathbf{n}^e) + P_j\Bigr] = 0 
\quad \Longrightarrow \quad 
\sum_{k=1}^{S}\frac{\partial f_j}{\partial n_k}\frac{dn_k^e}{dP_j} + 1 = 0.
$$

- **For species $i \neq j$:**

$$
\frac{d}{dP_j}f_i(\mathbf{n}^e) = 0 
\quad \Longrightarrow \quad 
\sum_{k=1}^{S}\frac{\partial f_i}{\partial n_k}\frac{dn_k^e}{dP_j} = 0.
$$

---

## Jacobian Matrix

Define the Jacobian matrix $A$ by

$$
A_{ik} = \frac{\partial f_i}{\partial n_k}.
$$

Then the differentiated equations for all species can be written compactly as:

$$
\sum_{k=1}^{S} A_{ik}\frac{dn_k^e}{dP_j} = -\delta_{ij}, \quad i = 1, 2, \dots, S,
$$

where the Kronecker delta $\delta_{ij}$ is given by

$$
\delta_{ij} =
\begin{cases}
1, & \text{if } i = j, \\
0, & \text{if } i \neq j.
\end{cases}
$$

---

## Invertibility of $A$

We assume that the Jacobian matrix $A$ is invertible. This assumption is justified because the original $\mathbf{n}^*$ is a stable equilibrium.

---

## Solving for the Equilibrium Shift

Define the vector of responses as

$$
\frac{d\mathbf{n}^e}{dP_j} =
\begin{bmatrix}
\frac{dn_1^e}{dP_j} \\
\frac{dn_2^e}{dP_j} \\
\vdots \\
\frac{dn_S^e}{dP_j}
\end{bmatrix},
$$

and let

$$
\mathbf{e}_j =
\begin{bmatrix}
0 \\
\vdots \\
1 \quad (\text{at the } j\text{-th position}) \\
\vdots \\
0
\end{bmatrix}.
$$

Then, the system can be written in matrix form as

$$
A \frac{d\mathbf{n}^e}{dP_j} = -\mathbf{e}_j.
$$

Assuming $A$ is invertible, the solution is:

$$
\frac{d\mathbf{n}^e}{dP_j} = -A^{-1}\mathbf{e}_j.
$$

This result quantifies how each species' equilibrium abundance shifts in response to a small, sustained press perturbation on species $j$.

---

[⬅️ Back](slide2_5.md) | [Next ➡️](slide3_2.md)