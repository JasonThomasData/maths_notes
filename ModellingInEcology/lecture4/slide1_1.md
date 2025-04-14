# Why use Press Perturbations?


<p align="center">
  <img src="PressPerturbation.jpg" 
       alt="Press perturbation illustration" 
       style="width:70%; height:auto;">
</p>
<br>

We often use the mathematics of **press perturbations** in ecosystem modelling to predict the effects of interventions. Why?

- ✅ They are **fast** to compute. Compared to simulating a system of ODEs to a new equilibrium, it's relatively easy to invert a matrix.
- ✅ They don’t require strong assumptions about **dynamical structure**. That is, if you want to simulate that system of ODEs, you need to define the equations (e.g. we need to assume the system is Lotka–Volterra). It's easier to jump straight to the Jacobian.
- ✅ They are **traditional** — easy to use, commonly accepted, and widely taught.

<br>
<p align="center">
  <img src="Raymond.jpg"
       alt="Press perturbation illustration" 
       style="width:70%; height:auto;">
</p>
<br>
<small>Raymond, Ben, et al. "Qualitative modelling of invasive species eradication on subantarctic Macquarie Island." Journal of Applied Ecology 48.1 (2011): 181-191.</small>

[Next ➡️](slide1_2.md)