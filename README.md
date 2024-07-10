# CasualXGBoost-for-TER

Terrestrial ecosystem respiration (TER) plays a critical role in global carbon budgets. However, the cross-region generalization ability of existing TER estimation models is limited due to the inherent variability of respiration processes among biomes and climates. In other words, due to the complex physical and biochemical processes underlying ecosystem respiration and the limited understanding of the factors driving TER variation, it is still hard to add rational constraints into TER estimation models.

Therefore, we developed a causality guided TER estimation framework by combining the XGBoost machine learning model with the PCMCI causal inference technique. In general, this framework can obviously modify the physical structure of XGBoost models and further guide and enhance models’ causal structure, resulting in lower upper limits of estimation error and higher spatiotemporal generalization ability. In addition, according to the results derived from SHAP, the causal structure of causal XGBoost model showed a generally consistent pattern with the “limiting factor theory” in ecology, and revealed the critical role of vegetation structure on TER estimation. 

![graphical_abstract](./plot/graphical_abstract.jpg)

![LeaveOneCrossVlidation](./plot/Figure_8.png)

![SHAP_Tair](./plot/Figure_10.png)