Inputs=InputSettings(
#T
[ 14 ] ,
#nItems
Int(325) ,
#nGroups
Int(1) ,
#Groups
[ "Main" ] ,
####################################################################
########################## IRT #####################################
####################################################################
#IRTmodel
"3PL" ,
#IRTparameters
[ "a", "b", "c" ] ,
#IRTparametrization
"at-ab" ,
#IRTD
1.0 ,
####################################################################
######################FRIENDS AND ENEMIES###########################
####################################################################
#EnemySetsVar
Vector{String}(undef,0),
#FriendSetsVar
[ "UNIT" ] ,
####################################################################
########################### ITEM USE  ##############################
####################################################################
#ItemUse_min
fill( 0 , 325 ) ,
#ItemUse_max
fill( 2 , 325 ) ,
####################################################################
########################## TEST LENGHT #############################
####################################################################
#lenght_min
[ 45 ] ,
#lenght_max
[ 45 ] ,
#lenght_weight
[ 1.0 ] ,
####################################################################
####################### EXPECTED SCORE #############################
####################################################################
#ExSVar
[""],
#ExSPts
[ zeros(Float64,1) ] ,
#ExS_min
[ [0.0] ] ,
#ExS_max
[ [1.0] ] ,
####################################################################
###################### GENERIC CONSTRAINTS#################
####################################################################
#meanVars
Vector{Vector{String}}(undef,0) , #(future)
#meanVars_min
Vector{Vector{Float64}}(undef,0) , #(future)
#meanVars_max
Vector{Vector{Float64}}(undef,0) , #(future)
#sumVars
Vector{Vector{String}}(undef,0) ,
#sumVars_min
Vector{Vector{Float64}}(undef,0) ,
#sumVars_max
Vector{Vector{Float64}}(undef,0) ,
####################################################################
######################### OBJECTIVE ################################
####################################################################
#OptType
"MAXIMIN" ,
#OptPts
[ [ zero(Float64) ] ] ,
#obj_targets (required in MINIMAX)
Vector{Vector{Float64}}(undef,0),
#AuxInt
zero(Float64) ,
#AuxFloat
zero(Float64) ,
####################################################################
######################### OUTPUT ###################################
####################################################################
#CATEGORIES
["UNIT",
		"Content_Domain",
		"Cognitive_Domain",
		"Item_Type",
		"New_Cycle",
		"Topic",
		"Biology_Knowing",
		"Biology_Applying",
		"Biology_Reasoning",
		"Chemistry_Knowing",
		"Chemistry_Applying",
		"Chemistry_Reasoning",
		"Physics_Knowing",
		"Physics_Applying",
		"Physics_Reasoning",
		"Earth_Knowing",
		"Earth_Applying",
		"Earth_Reasoning"
		]
);
