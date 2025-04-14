# Task Overview: Real Ecosystem Experiments

Each of you will work with one of these real ecosystem structures. Your job is to:

1. Use the interaction **structure** to generate parameter values:
   - Find combinations of growth rates and interaction strengths that result in a **feasible** and **stable** equilibrium (all species have positive abundance and the system is locally stable).

2. **Run two types of analysis** for each ecosystem instance:
   - **Press perturbation analysis**:
     - Analytically compute the Jacobian at equilibrium.
     - Use it (through its inverse) to predict how species abundances respond to small sustained changes in the growth rate of each species.
   - **Eradication experiments**:
     - Simulate what happens when each species is fully removed.
     - Let the system evolve to a new equilibrium (even if more species have to go extinct), and record the new species abundances.

3. For each pair of species $(i, j)$, extract:
   - The *qualitative* effect of applying a small press **perturbation** to species $j$ on the abundance of species $i$ (from the press perturbation matrix).
   - The *qualitative*  effect of **removing** species $j$ on the abundance of species $i$ (from the simulation).

This gives us two *qualitative* predicted outcomes for the same type of intervention into the same ecosystem structure.

[⬅️ Back](slide2_2.md) | [Next ➡️](slide2_4.md)

<br><br>
<p align="center">
  <img src="Process.jpg" 
       alt="Real and simulated food webs" 
       style="width:140%; height:auto;">
</p>
<br>
