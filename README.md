# Multimodal-RL-preparation
The code provided here was used to prepare and estimate the multimodal route choice model presented in Meyer de Freitas et al. (2018)

The Create_Edges.R file is used to create a static transit network as well as transfer connections in this transit network based on the dynamic transit network written from the R5 transit router. 

The DataPrep_Hooper_R5_car_slow_pt.R file is used to create the supernetwork, combining the static transit network to the street network. This code also matches the observations stemming from the R5 transit routes to the static networks. By combining the different networks, the separate stages of the same trip (which are routed in separate routers in Java) are comb ined to form a single line of link ID's in this code. 
