# Testing It: Real and Simulated Food Webs

We’ll measure the mismatch using two approaches:

1. **Simulated food webs** (randomly generated structure, randomly generated parameters).
2. **Real food webs** from literature (known structure, randomly generated parameters).

Let's start by focusing on the second approach. We need to:
- Choose an ecosystem **structure** from the literature. This is basically a sign-structured matrix $A$ where $[A]_{ij} = a_{ij}$ that tells us which interactions are positive, which are negative, and which are zero.
- Generate ecosystem models (structure and parameter values) that have a **feasible** and **stable** equilibrium
- Run **press perturbations** around the equilibrium.
- Run **extinction simulations** from the same equilibrium using a full (LV) model.
- For each pair of perturbed and response species, we can measure if the press perturbation predictions agree with the extinction simulations.
- Propose we start with _agreement_ in the qualitative sense.

[⬅️ Back](slide2_1.md) | [Next ➡️](slide2_3.md)

## Network 1: Booderee National Park
<br> <br>
<p align="center">
  <img src="InteractionNetworks/BoodereeInteractionNetwork.jpg" 
       alt="Dynamics intuition figure" 
       style="width:70%; height:auto;">
</p>
Baker, Christopher M., et al. "A novel approach to assessing the ecosystem-wide impacts of reintroductions." Ecological Applications 29.1 (2019): 1-12.
<br>

## Networks 2 & 3: Yellowstone National Park & Semi-arid Australia
<br> <br>
<p align="center">
  <img src="InteractionNetworks/Yellowstone_SemiAridAustralia.jpg"
       alt="Dynamics intuition figure" 
       style="width:100%; height:auto;">
</p>
Vollert, Sarah A., Christopher Drovandi, and Matthew P. Adams. "Unlocking ensemble ecosystem modelling for large and complex networks." PLoS Computational Biology 20.3 (2024): e1011976.
<br>

## Network 4: Christmas Island World Heritage Area
<br> <br>
<p align="center">
  <img src="InteractionNetworks/ChristmasIslandInteractionNetworks.jpg"
       alt="Dynamics intuition figure" 
       style="width:70%; height:auto;">
</p>
Kristensen, Nadiah P., Ryan A. Chisholm, and Eve McDonald‐Madden. "Dealing with high uncertainty in qualitative network models using Boolean analysis." Methods in Ecology and Evolution 10.7 (2019): 1048-1061.  

Han, Yi, et al. "Predicting the ecosystem-wide impacts of eradication with limited information using a qualitative modelling approach." Ecological Modelling 430 (2020): 109122.
<br>

## Network 5: Devil reintroduction to mainland Australia
<br> <br>
<p align="center">
  <img src="InteractionNetworks/TasmanianDevilReintroduction_Fig.jpg"
       alt="Dynamics intuition figure" 
       style="width:70%; height:auto;">
</p>
Hunter, Daniel O., et al. "Reintroduction of Tasmanian devils to mainland Australia can restore top-down control in ecosystems where dingoes have been extirpated." Biological Conservation 191 (2015): 428-435.
<br>

## Network 6: Dirk Hartog Island reintroductions
<br> <br>
<p align="center">
  <img src="InteractionNetworks/DirkHartogIsland.jpg"
       alt="Dynamics intuition figure" 
       style="width:70%; height:auto;">
</p>
Peterson, Katie A., et al. "Reconstructing lost ecosystems: A risk analysis framework for planning multispecies reintroductions under severe uncertainty." Journal of Applied Ecology 58.10 (2021): 2171-2184.
<br>

## Network 7: Great Barrier Reef
<br> <br>
<p align="center">
  <img src="InteractionNetworks/GreatBarrierReef.jpg"
       alt="Dynamics intuition figure" 
       style="width:70%; height:auto;">
</p>
Vollert, Sarah A., Christopher Drovandi, and Matthew P. Adams. "Unlocking ensemble ecosystem modelling for large and complex networks." PLoS Computational Biology 20.3 (2024): e1011976.
<br>

## Network 8: Phillip Island fox eradication
<br> <br>
<p align="center">
  <img src="InteractionNetworks/PhillipIsland.jpg"
       alt="Dynamics intuition figure" 
       style="width:70%; height:auto;">
</p>
Vollert, Sarah A., Christopher Drovandi, and Matthew P. Adams. "Unlocking ensemble ecosystem modelling for large and complex networks." PLoS Computational Biology 20.3 (2024): e1011976.
<br>

## Network 9: Malleefowl management
<br> <br>
<p align="center">
  <img src="InteractionNetworks/MalleefowlInteractionNetwork.jpg"
       alt="Dynamics intuition figure" 
       style="width:100%; height:auto;">
</p>
Bode, Michael, et al. "Revealing beliefs: using ensemble ecosystem modelling to extrapolate expert beliefs to novel ecological scenarios." Methods in Ecology and Evolution 8.8 (2017): 1012-1021.
<br>

## Network 10: Macquarie Island
<br> <br>
<p align="center">
  <img src="InteractionNetworks/MacquarieIslandInteractionNetwork.jpg"
       alt="Dynamics intuition figure" 
       style="width:100%; height:auto;">
</p>
Raymond, Ben, et al. "Qualitative modelling of invasive species eradication on subantarctic Macquarie Island." Journal of Applied Ecology 48.1 (2011): 181-191.
<br>

