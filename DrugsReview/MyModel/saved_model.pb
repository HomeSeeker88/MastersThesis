??
?&?&
D
AddV2
x"T
y"T
z"T"
Ttype:
2	??
B
AssignVariableOp
resource
value"dtype"
dtypetype?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
K
Bincount
arr
size
weights"T	
bins"T"
Ttype:
2	
N
Cast	
x"SrcT	
y"DstT"
SrcTtype"
DstTtype"
Truncatebool( 
h
ConcatV2
values"T*N
axis"Tidx
output"T"
Nint(0"	
Ttype"
Tidxtype0:
2	
8
Const
output"dtype"
valuetensor"
dtypetype
?
Cumsum
x"T
axis"Tidx
out"T"
	exclusivebool( "
reversebool( " 
Ttype:
2	"
Tidxtype0:
2	
R
Equal
x"T
y"T
z
"	
Ttype"$
incompatible_shape_errorbool(?
=
Greater
x"T
y"T
z
"
Ttype:
2	
?
HashTableV2
table_handle"
	containerstring "
shared_namestring "!
use_node_name_sharingbool( "
	key_dtypetype"
value_dtypetype?
.
Identity

input"T
output"T"	
Ttype
l
LookupTableExportV2
table_handle
keys"Tkeys
values"Tvalues"
Tkeystype"
Tvaluestype?
w
LookupTableFindV2
table_handle
keys"Tin
default_value"Tout
values"Tout"
Tintype"
Touttype?
b
LookupTableImportV2
table_handle
keys"Tin
values"Tout"
Tintype"
Touttype?
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
?
Max

input"T
reduction_indices"Tidx
output"T"
	keep_dimsbool( " 
Ttype:
2	"
Tidxtype0:
2	
>
Maximum
x"T
y"T
z"T"
Ttype:
2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(?
>
Minimum
x"T
y"T
z"T"
Ttype:
2	
?
Mul
x"T
y"T
z"T"
Ttype:
2	?
?
MutableHashTableV2
table_handle"
	containerstring "
shared_namestring "!
use_node_name_sharingbool( "
	key_dtypetype"
value_dtypetype?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
?
PartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
?
Prod

input"T
reduction_indices"Tidx
output"T"
	keep_dimsbool( " 
Ttype:
2	"
Tidxtype0:
2	
?
RaggedTensorToTensor
shape"Tshape
values"T
default_value"T:
row_partition_tensors"Tindex*num_row_partition_tensors
result"T"	
Ttype"
Tindextype:
2	"
Tshapetype:
2	"$
num_row_partition_tensorsint(0"#
row_partition_typeslist(string)
@
ReadVariableOp
resource
value"dtype"
dtypetype?
E
Relu
features"T
activations"T"
Ttype:
2	
?
ResourceGather
resource
indices"Tindices
output"dtype"

batch_dimsint "
validate_indicesbool("
dtypetype"
Tindicestype:
2	?
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
A
SelectV2
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
N
Squeeze

input"T
output"T"	
Ttype"
squeeze_dims	list(int)
 (
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ??
@
StaticRegexFullMatch	
input

output
"
patternstring
m
StaticRegexReplace	
input

output"
patternstring"
rewritestring"
replace_globalbool(
?
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
<
StringLower	
input

output"
encodingstring 
e
StringSplitV2	
input
sep
indices	

values	
shape	"
maxsplitint?????????
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?"serve*2.7.02v2.7.0-rc1-69-gc256c071bb28??
?
embedding_6/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape:
?N?*'
shared_nameembedding_6/embeddings
?
*embedding_6/embeddings/Read/ReadVariableOpReadVariableOpembedding_6/embeddings* 
_output_shapes
:
?N?*
dtype0
{
dense_13/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?* 
shared_namedense_13/kernel
t
#dense_13/kernel/Read/ReadVariableOpReadVariableOpdense_13/kernel*
_output_shapes
:	?*
dtype0
r
dense_13/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_13/bias
k
!dense_13/bias/Read/ReadVariableOpReadVariableOpdense_13/bias*
_output_shapes
:*
dtype0
z
dense_12/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:* 
shared_namedense_12/kernel
s
#dense_12/kernel/Read/ReadVariableOpReadVariableOpdense_12/kernel*
_output_shapes

:*
dtype0
r
dense_12/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_12/bias
k
!dense_12/bias/Read/ReadVariableOpReadVariableOpdense_12/bias*
_output_shapes
:*
dtype0
f
	Adam/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_name	Adam/iter
_
Adam/iter/Read/ReadVariableOpReadVariableOp	Adam/iter*
_output_shapes
: *
dtype0	
j
Adam/beta_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_1
c
Adam/beta_1/Read/ReadVariableOpReadVariableOpAdam/beta_1*
_output_shapes
: *
dtype0
j
Adam/beta_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_2
c
Adam/beta_2/Read/ReadVariableOpReadVariableOpAdam/beta_2*
_output_shapes
: *
dtype0
h

Adam/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name
Adam/decay
a
Adam/decay/Read/ReadVariableOpReadVariableOp
Adam/decay*
_output_shapes
: *
dtype0
x
Adam/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *#
shared_nameAdam/learning_rate
q
&Adam/learning_rate/Read/ReadVariableOpReadVariableOpAdam/learning_rate*
_output_shapes
: *
dtype0
n

hash_tableHashTableV2*
_output_shapes
: *
	key_dtype0*
shared_name219087*
value_dtype0	
?
MutableHashTableMutableHashTableV2*
_output_shapes
: *
	key_dtype0*
shared_nametable_208871*
value_dtype0	
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0
?
Adam/embedding_6/embeddings/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
?N?*.
shared_nameAdam/embedding_6/embeddings/m
?
1Adam/embedding_6/embeddings/m/Read/ReadVariableOpReadVariableOpAdam/embedding_6/embeddings/m* 
_output_shapes
:
?N?*
dtype0
?
Adam/dense_13/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*'
shared_nameAdam/dense_13/kernel/m
?
*Adam/dense_13/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_13/kernel/m*
_output_shapes
:	?*
dtype0
?
Adam/dense_13/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*%
shared_nameAdam/dense_13/bias/m
y
(Adam/dense_13/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_13/bias/m*
_output_shapes
:*
dtype0
?
Adam/dense_12/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*'
shared_nameAdam/dense_12/kernel/m
?
*Adam/dense_12/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_12/kernel/m*
_output_shapes

:*
dtype0
?
Adam/dense_12/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*%
shared_nameAdam/dense_12/bias/m
y
(Adam/dense_12/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_12/bias/m*
_output_shapes
:*
dtype0
?
Adam/embedding_6/embeddings/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
?N?*.
shared_nameAdam/embedding_6/embeddings/v
?
1Adam/embedding_6/embeddings/v/Read/ReadVariableOpReadVariableOpAdam/embedding_6/embeddings/v* 
_output_shapes
:
?N?*
dtype0
?
Adam/dense_13/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*'
shared_nameAdam/dense_13/kernel/v
?
*Adam/dense_13/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_13/kernel/v*
_output_shapes
:	?*
dtype0
?
Adam/dense_13/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*%
shared_nameAdam/dense_13/bias/v
y
(Adam/dense_13/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_13/bias/v*
_output_shapes
:*
dtype0
?
Adam/dense_12/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*'
shared_nameAdam/dense_12/kernel/v
?
*Adam/dense_12/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_12/kernel/v*
_output_shapes

:*
dtype0
?
Adam/dense_12/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*%
shared_nameAdam/dense_12/bias/v
y
(Adam/dense_12/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_12/bias/v*
_output_shapes
:*
dtype0
G
ConstConst*
_output_shapes
: *
dtype0	*
value	B	 R
H
Const_1Const*
_output_shapes
: *
dtype0*
valueB B 
I
Const_2Const*
_output_shapes
: *
dtype0	*
value	B	 R 
I
Const_3Const*
_output_shapes
: *
dtype0	*
value	B	 R 
??
Const_4Const*
_output_shapes	
:?N*
dtype0*۫
valueЫB̫?NBiBandBtheBtoBmyBaBitBforBwasBhaveBofBonBthisBinBbutBmeBhadBisBwithBthatBnotBsoBbeenBatBafterBnoBamBasBaboutBdayBimBnowBsideBtakingBallBhasBiveByearsBpainBfirstBeffectsBtakeBmonthsBonlyBstartedBlikeBgetBbeByouBupBdaysBjustBveryBitsBtimeBfromBorBoutBwhenBfeelBifB2BwouldBbecauseBbackBpillBmoreB3BoneBweeksBareBweekBalsoBtookBperiodB
medicationBdoctorBweightBbeforeBgotBthenBwillBdidBdoBmonthBwhichBanBsinceBdontBcanBlifeBstillBbadBanyBoffBreallyBmuchBsomeBanxietyBoverBneverBthanBwentBbetterBmedicineBtwoBworkBotherBfeltBtheyBwellBcontrolBeveryBdidntBgoodByearBevenBgreatBfewBhavingBnightBgoBsleepBagainBagoBbyBwereBtriedBworksBacneBbirthB4BmadeBlittleBdrugBlastBhoursBgoingBdoseB
depressionB
prescribedBworkedB5BusedBwhatBalmostBmgBcouldBgettingBsevereBdownBputBhelpByourB6BthereBfeelingBheB10BuseBbeingBfarBeverBmoodBskinBampBnothingBperiodsBawayBstoppedBlotBnauseaBusingBsexBtryBtimesBtooBcrampsBsymptomsBbleedingBknowBthinkB
experienceBhelpedBlostBgainBnormalB	recommendBwithoutBwhileB1BcantBeffectBlongBpillsBhoweverBworseBthemBoldBsayBmorningBbodyBuntilBstopBwithinBmostBtakenBseeBthingBdoesBhowBalwaysBmanyBhorribleBonceBnextBmakeBaroundBexperiencedBtheseBthroughBhaventBdueBableBbloodBrightB	headachesBnoticedBfinallyBstomachBgoneBsheBgaveBthreeBwantBmyselfBlaterBloveBbestB	differentBeatBsureBpoundsBthoughtBworkingBduringBsameBcouldntBthoughBfoundBproblemsBwhoBgainedBlessBdailyBanythingBhappyBpeopleBgiveBsecondBsaidBwayB
completelyBneedBtoldBswingsBanotherB12BthingsB	somethingBcoupleBweBintoBfaceBstartB
everythingB20BhalfBworstBspottingBnewB8BdriveBworthBtiredBhelpsBbitBmayB7BstartingBterribleBwasntBproblemBreliefBshotBpanicBprettyBlossBpainfulBhighBdryB	extremelyBlongerBpastBappetiteBlightB	effectiveBactuallyBminutesB
definitelyBlbsBdecidedBtodayBpregnantBhopeBwhereBdrB30BissuesBcameBkeepBhourBtryingBwaterBdoesntBreviewsBheavyBbedBmakesBcrampingBchangedB15B	diagnosedBfindBresultsBmedsBswitchedBtwiceB	migrainesB	sometimesBgivenBseveralBheadacheBchangeBfullB	infectionBlowBcausedBanyoneBthatsBfineBenoughB	depressedBclearBattacksB	insuranceB	increasedBdoneBmouthBhardBpressureBseemsBbeganBsickBshouldBenergyBdosageBreadBproductBthoseBbothBalreadyBleastB	treatmentBeatingBeveryoneBcomeBpointBamazingBillBidBelseB
differenceB	insertionBmildBheartBneededBoverallBwantedBnegativeBmaybeBrecentlyBsoonBbecameBfreeBinsomniaB
absolutelyBherBhairBawfulBusuallyBdiarrheaBextremeBmedBanymoreBstayBdoingBdrinkBleftBfoodBmedicationsBperBwholeBothersBloseBimmediatelyBsufferedBinsertedBdoctorsBeachBsmallBetcBendBtellB	medicinesBcauseBmajorBhereBconstantByetBlowerBalthoughBdisorderBcoldBtasteBalongBplanBcrazyBmiracleB25BpersonBchronicBmirenaBsleepingBpatchBjobBnauseousB9BquitBreasonBwhyBlastedBheadBsuchBgottenBeverydayB100B	pregnancyBlexaproBzoloftBthoughtsBreadingBremovedBbelieveBdietBbigBburningBwakeBpositiveBmigraineBhopingBregularBpackB24BslightBasleepBclearedBhighlyBwokeBhurtBexerciseB	nexplanonBpartBprescriptionBrememberBunderBshortBbcBmuscleBseemBwishBsurgeryBfourB
constantlyBconstipationBerBstuffBcontinueBhomeBnoticeBfatigueB10mgBhisBprobablyBtakesBquiteBeitherBdealBmindBschoolBexperiencingBrecommendedBdocB50BsoreBbreakBincreaseBthankB	dizzinessBletBhimBdizzyBchestBseemedBitchingBsufferBkeptBwaitBearlyB23BhospitalB	emotionalBhotB	literallyBquicklyBmightBplusBcycleBhugeBwouldntBhusbandBendedBsuperBiudBexceptBcreamBthirdBgenericBstressB18BxanaxBwontBthinkingB	beginningBscaredByrsB	injectionBpainsBvomitingBanxiousB	developedBcareBcomingB	sufferingBfastBeasyBfamilyBimprovementBlegsBbipolarBbetweenBmakingBcannotBgoesBimprovedBimplantB40BbecomeBattackBgladBimplanonBhealthyBeyesBentireBfunctionBcymbaltaB20mgBisntByesB	wonderfulBpleaseBfallBcoughBageBlevelBluckBswitchBmoodyBliveB	currentlyBworryBlookingB	boyfriendB
especiallyB	expensiveBwalkBneckBtroubleBbrainBpriorB2ndBtotallyBhonestlyBbBinsteadBamountB14BsavedBhandsBprozacBoftenBcryingBredBgetsBdrugsBfactBbarelyBcourseBfeelsBstraightBissueBnearlyB3rdBcalledBcostBbreastsBsonBtypeBeffexorBdrinkingB
withdrawalBsixBhungryBfeetBratherBrealBhappenedBreactionBbathroomBdepoBrestBkindByeastB2015BdreamsBlevelsB
wellbutrinBdosesBlateBactiveB1stBmustBaskedB45B50mgBchangesBlotsBsomeoneBsmokingB	yesterdayBslowlyBbreastBtheirBsawB34B	hopefullyBweirdBsugarBguessBintenseBhormonesBreducedBwondersBselfBhateBbloatingBarmBnormallyBbabyBlosingBlookBcravingsBknewB16BworriedB	suggestedBpayBgodBsleepyBtestB	decreasedBlegBunfortunatelyBfiveBpaxilBkidsBbrokeBcutB	switchingBsizeBskylaBbowelBgivingBhelpingBminorBnoneBridBhealthBformBpossibleBokBtabletsBbrandBslightlyB
eventuallyBcomesBseriousBaffectsBtotalBcaseBnervousBnoseBstrongBownBlovedBcalmBnightsB	continuedBcelexaB5mgBsupposedB	procedureBuncomfortableBfibromyalgiaBsweatingBfocusBsuicidalBseenByazBupsetBbleedB
injectionsBlighterB	difficultBadhdBsystemBokayBswellingB100mgBsleptBleaveBsingleB	irregularB	sensitiveBthroatBcryBhasntBzeroBchildBsaysBkneeByoureBtrulyBstickBturnedB
occasionalBflashesBminimalBeasilyBtermBawakeBchantixBseroquelBareaBseeingBreviewBdermatologistBcausingBusualBpostBtabletBrateB	irritableB
unbearableBeveningB11BvisionBallergicBwearBthanksB
throughoutBstoppingBachesBloestrinB2016BnuvaringBrashBlibidoBhellBourBaddBwrongBantibioticsB	arthritisBsocialBdiseaseB	followingBeyeB	conditionBpimplesB13BunableBhappenBambienBmostlyB
infectionsBperfectBfriendsB	miserableBhrsB25mgBsyndromeB60BjointBcombinationBadderallBhandleBantidepressantsBmoveB
discomfortBcontraveBdaughterBfemaleBbledBangryBgivesBawesomeBnerveBlastsB	breakoutsBxrBhitBklonopinBcloseBlyricaBafraidBitchyBhigherBlackBeasierBplaceBhouseBsinusBbloatedBtalkBaskBmineBcausesBcysticBreadyBonesBpmBpristiqBanywayBrunBcancerBfinishedBbreathBbreakthroughBmarchBsadBwalkingBrealizedBalcoholBstayedBbottleBtreatB	initiallyB	includingBsuboxoneBcompleteB17BshotsBmiddleBcoverBmemoryBchildrenBpharmacyBhardlyBsexualBmoderateBhandBheardBtenB35BtherapyBjuneBcallBprocessBfluBwakingBfeBkeepsBjulyBmatterBfallingBmoneyBswollenB	ibuprofenBhappierB
antibioticBresearchBateBappointmentBsmokeB	basicallyBinsideBhelpfulBloBwaitingBextraBaddedBusBchangingBbedtimeBmultipleBquickBforgetBexactlyBcheckB	dischargeBlookedBcomparedB
noticeableBmentalBpriceBmissBprepBniceBspotBantidepressantBbladderB75BsubsidedBqualityBideaBphysicalB21B40mgBannoyingBxB90BpsychiatristBgeneralBspasmsBdroppedBtopBsweatsB	otherwiseBavoidB	abdominalBaccutaneBreturnedBfeverBvyvanseBhormonalBsignificantBfeelingsB
previouslyB30mgBcollegeBtramadolBwifeBsprintecBlamictalBabilifyBlivingBnameBfrequentBseizuresBwomenBsuccessBnonBdrankBdealingBsweatBaugustBorthoBjanuaryBpleasedBunprotectedBtomorrowB2014BmeanB4thBstandBdrowsyBaffectBaprilBmoodsBroomBboneBgelBarmsBcarefulButiB19BexperiencesB	psoriasisBresultB	toleranceBpassedBflowBdecreaseBconsideringBupdateBdisappearedBtestsBmealBinitialBoptionBlargeBwonderBsaverBmaleB	breathingBgreatlyBcoveredBbpBpmsBemotionsBweighB	prescribeBexpectedB	seriouslyBfearBpatchesBweighedBsignificantlyBrealizeBtogetherBplacedBsomewhatBracingBchinBstuckBrelatedBaloneBsitBoccasionallyBboughtBdeepBbesidesBsayingBpreventBmomBbelviqB
gabapentinBacidBepisodesB150BspotsBasthmaB
whatsoeverBspentBtalkingBgasBsiteBringBpreviousBenjoyBminsBmethodBgainingBepiduoBcolonoscopyBcondomBcleanBmarketBsimilarBbreakingBchanceB
understandBliverB
drowsinessBchoiceBohBdropBexcitedBscaryBslowBnearBrestlessBivBadviceBthyroidBoctoberByourselfB	tirednessBfellBkickBfriendB150mgBocdB	excellentBnurseBcounterBtopamaxBtinglingB
nightmaresBthankfulBwomanBcupBreduceBlooseBkneesBhivesBfollowedBtheresBburnBdifferentlyB200BbeginBdecemberBweakBbenefitsBativanBphentermineBmedicalB	allergiesB28BdespiteBdesireB22BapplyBupperBworldB
afterwardsBsuggestB	struggledBvividB	methadoneBofficeBrandomB	menstrualBreceivedBmissedBrelationshipBlipsB	availableBmotherBforwardBcoughingBzombieBstrangeBwriteBtillBdarkBoralBmentionBaffectedBpassBunlessBdownsideBcarBwowB	sensationB	immediateBitchBrarelyBkidneyBfingersB	importantBlooksBhappensB	afternoonBtoiletBrunningBangerBwhiteBseverelyBfoodsBpatientBkeepingB
difficultyBfinishBeffectivenessBbuyB	metforminBvaginalBnastyBdamageBcystsB	concernedBstateB80BstableB	exhaustedB	regularlyBmonistatBhumiraB300BflareBdecisionBmovementBdebilitatingB75mgBtenderBmainBshareBputtingBconsBtolerateBluckyBlastingBnovemberBitselfBrednessBgratefulBemptyB	nauseatedBrelievedB	fantasticB
prednisoneBdrivingB
tendernessBcompanyBsmellBsenseBsecondsB	septemberBpercocetB
motivationBnumbB
manageableBdinnerBopenBchillsB	nightmareBhatedBfollowByrBlatudaBvictozaBarentBminBcrampBbroughtBshoulderBestrogenB
treatmentsBturnByoungBtrueBfebruaryBtreatedB	tolerableBstartsBspottedBfindingBcontrolsBbiggerB	heartburnB
controlledBtylenolBabilityB	emergencyBconstipatedBcureB2xBfairlyB2013BearBwithdrawalsBheavierB300mgBroundBstayingBsetBranBnumberB
increasingBcommonBcommentsBlayBhurtsBawareBladiesBgoalBknownBcervixBvaginaBfootBfailedBmanicBhormoneBdrynessBaccidentBuppedBreturnBminuteBsentB	oxycodoneB
complaintsBneedsBmovedBwantingBfullyBsmokedBapproximatelyBprosBaffordBmovingBbiggestBriskBforeheadBepisodeBsuddenlyBacheBvaliumBroughBhistoryBgpBfiguredBfogBclearingBadjustB	generallyBthrowB48B
strugglingBliquidBcBstopsBeaseB	addictionB	mentionedB
incrediblyBjointsBsamplesBsafeBonlineBallergyBtremendouslyBlikelyBwashBvicodinB2017BexpectBstoryBneurologistBmixedBconcentrateBreleaseBmusclesBstoriesBnaturalBrelieveBholdBtinyBirritabilityB	excessiveBhadntB	graduallyBurgeBmessBsuddenB	continuesB1mgBa1cBdiscBwhetherBtrialB500mgBmondayB120B	reactionsBcholesterolBcertainBpartnerBlolBregulateBadditionBgymBcheeksBsittingBnorcoBhipBcoffeeBnorBparagardBjawB	complaintBbadlyBwantsBsolutionBfatB26BthrewB	physicianBbusparB200mgBviibrydBsendB
impossibleBseizureBpatientsB
originallyBteethB	oxycontinB
exercisingBobgynBbecomingBsicknessBawhileBrelaxBgroggyBboobsBmorphineBbreatheBshakingBexcruciatingBwatchBstressedBmanBmgsB56BdieBopinionBrelaxedBotcBssrisBmistakeBremoveBcriedBclotsB	situationBthrowingBsimplyBtalkedBbactrimBadultBnasalBedgeBurineBearlierB15mgBmanageBerectionBsummerBsoresBibsBvisitBuponBmonthlyB
personallyBstoolBcialisB	surgeriesBalotBallowedBloweredB2012BfunBtouchBbreakoutB5thBhungerBconcertaBspinalBfridayBfireBtestedB	terrifiedBoptionsBlithiumBfocusedB
pharmacistBmomentBsortB	perfectlyBscaleBagainstBwearingB
preventingB	breakfastBbenadrylBcheckedBlineBendometriosisBaboveBlearnedB	satisfiedBsmallerBappliedB60mgBmidBearsBsideeffectsB	surprisedBstageBbenefitB27BwaitedByasminBwasteBhearBfixBdiabetesBconsiderBoutsideB
depressiveBuBknowingBmainlyBintercourseBimproveBturkeyBhabitsBalternativeBcomfortableBiceBfigureBclinicB36BptsdBmsBblackBnumerousB	diagnosisBurinaryBsubsideB	everyonesBbrownBsunBorderBnoticingBbasisBciproBadverseB500ByoullBshorterB	irritatedBrangeBirsquomBtheyreB
bronchitisBaddictedBintakeBinhalerBrareBproductsBovarianB
frequentlyBcountBprogramBmealsB	certainlyBweaknessBapartBversionBvariousBstoneBroutineBpcosBforeverBadvisedBstrengthBoilyBoilBcuredBbotherBupsBheadedBfasterBbrokenB2011BwerentBtrinessaBdisappointedBrefluxBrapidBplaceboBtongueBlinzessBvomitBdepakoteB
confidenceBaverageBhopefulBsoonerB	instantlyB	moodinessBantiBscalpBinsaneBsymptomBimagineBdiscontinuedBshowedBsharpBpalpitationsB	obviouslyBbalanceBadvilB46BglassBmassiveBlistBdropsButerusBmanagedBfutureB
citalopramBbumpsBsugarsB2mgBpadBnoteBdBsampleB	happeningBsuprepBsevenB	movementsBcaloriesBabsoluteBtellingB
irritationBfourthB
specialistBshowerBchoseBspineB3xBspeakBcopayBsucksB	lifesaverBdoubleBcravingBmeansBritalinBoddBdateBdiabeticBinflammationBsprayB
physicallyBlatelyBnonstopBadipexBwhateverBhurtingBtriBsuicideBstruggleB	wonderingBactualBhiBpoorBhallucinationsBgirlB	trazodoneBsupplyBstrongerBcombinedBareasB	addictiveByiBlikedB
congestionBwheneverBviralBtrustBblurredBtearsBpoundBgradeBplentyBbotoxBpimpleBavianeB2010B
sleepinessBsimpleBmorningsBloadBgonnaBworsenedBproperlyBnumbnessBagreeBtrickBluteraBtopicalBparanoidBnapBsaxendaBmentallyBdeathBscheduleBluckilyBenbrelB	shouldersB
sertralineBinsulinBfacialBlunchB
apparentlyBpersonalityBpainlessBfitB
thankfullyBshowBwBforgotB
associatedBxlBtonightBsundayBcystBpossiblyB
managementBletsBdilaudidBscriptB32BremovalB	lifestyleBcontrollingBlowestBdirectedBforceBfatiguedBanywhereBsaveBtrackB	stressfulBworryingBgynoBconcentrationBgeneralizedBtastesBemotionallyBanywaysBamazedBunlikeBstoreBscarsBnumbersBcyclesBfreakingBdyingB	confidentBimitrexBhorrorBtoughBjitteryBclassBallowsB	obsessiveBpaidBbottomBmicrogestinBapplyingBreplacementB
discoveredB2009BdramaticallyBclearlyB70BwillingB
neuropathyBharderBthinBskipBactingB29BinsertBboutsBorgasmBinjuryB	attentionBthusBkilledBcardBtendBhorrificBvitaminBtreatingBstepBtractB
everywhereBbearableBpleasantBsupposeBburnsBbasedBtypesBlistedBcontraceptiveBadmitB
cigarettesBburnedBpeeBdiscontinueB
consistentBbeatBopiatesB	lethargicB	skepticalBfightBbringBwarningBtremorsBneedlessBirsquoveBinterestBconsistentlyBasideBworeBvertigoBuncontrollableBthruB	shortnessBratingBhospitalizedBreasonsB247BmagicBzofranBleavingBvirusBpulledBconcernBtonsBwordsBrxBcondomsBbeyondBwritingBapriBviagraBsavellaB	dangerousBclothesBonsetBsexuallyBwaysB
situationsBruinedBextendedBapplicationB0BlunestaBsmoothBrlsBmaxBbvBtonBreactBozBcreamsB
clonazepamBrequiredBfluidBlungsB6thBpersonalBnuvigilBfrustratingBscareBreportBfilledBmethotrexateBcallingBkickedBblueBasapBrecentBsmokerBquittingBmixBgiBmarkBmaniaBdrasticallyBolderBhopesBcouponBabnormalBreducingBmarriedBtripBopiateBweekendBperhapsBlightheadedBsignsBjunelBswallowBdrsB	menopauseB57BzyprexaB
ridiculousB	tricyclenBreplacedBexactBstrawBsignBshortlyBmissingBlipBbmBpublicBpushBfoggyB
unpleasantB	neurontinBgrowingBagitatedB	convincedBtrisprintecBtamponsBprimaryBmdBclearerBwarnedBregimenBlieBtamifluBstaysBinstructionsBheavilyBboxBmiralaxBfreakedBspreadBmilkBmadBhesitantBbehaviorBadjustedB
seasoniqueBcrapBblownBaliveB
additionalBopanaBkeppraBbornBacuteB
overweightBeightBbelowBanklesB1000BgrewBgoodnessBdoxycyclineBdiscsBcomplainBnucyntaB31BterriblyBsweetB	scheduledBrecoveryB
incredibleBgadBsaturdayBremeronBboyBdysfunctionBconfusedBcarryBtamponB	motivatedBhonestBhabitBssriBsweetsBregretByoBweanBbehindBabrevaBweeklyBproneBplacesBmaintainBlungBtubeBstudentBpacksBfurtherBworkoutBlemonBjuiceB	cigaretteBallowB	stiffnessBlessenedBhorriblyBdiedBgeodonBtowardsB	ovulationBhealingBfightingBdealtBcopeBquotnormalquotBknowsBfebBamitriptylineBnBhydrocodoneB
frustratedBsolidBkillBteenagerB
rheumatoidBpassingB
conditionsBaidBparanoiaBontoBgoutBtBomgBnonexistentBillnessB	bacterialB2008BsomaBreachB	reductionBdreamBdeadBcurrentB	overnightBkidBtightBlovingBcalmedBpmddBpayingBneedingB	expectingB
successfulBsensitivityB
productiveBpickB	urinationBtabsB	synthroidBosteoarthritisBitrsquosBabdomenB375BwhatsBsteroidB
convenientB	stratteraBspellsBkindaBcrohnsBaskingB
consideredBactivityBtoesBfrontBfentanylB
activitiesB	virtuallyBrideBmessedBforcedBtoothBplayBinfusionBinstantBfloorB	finishingB68BinformationBagreedB20sBpsychBrosaceaB
relativelyBoppositeBmakeupB	herniatedBdescribeBapproxB	traumaticB
horrendousB	frequencyBwonderfullyBstupidBjanBhealBguysBanswerBraisedBpreparedBpadsBleadBthickBsaphrisBpresentBmilesBmotionBledBkindsBeczemaBcaffeineB125BvaltrexBurinateB	trulicityBsoundBoutweighBhesBstatedBlargerBincludedBcuttingBnervesB	confusionBcomboBapprovedBunusualBspendBreactsBqsymiaBcalmerB400BbuildBrandomlyBraBlocalB	everybodyBprogressBhipsB
diminishedBteaB	inabilityBdegreeBdecideB05BimmuneBchooseBwearsBpocketBcheaperBcarbsBcapsulesBalertBrageBpostedBhangBblurryBembarrassingBturnsBnailsBchemoB65B	pneumoniaBdegenerativeBbeyazBsteroidsBremicadeB	regulatedBblessingB80mgBroofB	permanentBnarcoticBlimitedBslowedBmoisturizerBweanedBstronglyBprayBliftedBtuesdayBstrokeBstoolsBsorenessBrememberingBmetallicBacrossBtensionBpullB55BwatchingBneitherBwarmBlivedBheyBarmourBfunnyBcolonBadviseBsatBpreferBgirlsBgerdBcd4B
brintellixBuselessBheatB67BhearingBcrossedBabuseBshockedBdevelopBtypicalBtoeB	desperateB	adjustingBtripsBieBfdaBcyclenBstudyBrowBrefillBreachedBphoneBgodsendBflomaxBbrightBvomitedBtaperBprovigilBlistenB	erectionsBdulcolaxB	dependingBthursdayBtestosteroneBknockedBdoubtBbacteriaB38BwebsiteBscanBproveraBgumB	affectingBstiffBsnackBcoversB
aggressiveBpeelingBlayingB	impressedB	cripplingBcomplainingBoccurBitllB	narcoticsBfiberBconjunctionBcapsuleBbabiesByogurtBtasksB	naturallyBcraveB	clonidineBoutbreakBinternetBtastedBmarriageB	healthierB	thereforeBhighestBexplainBbruisingBgrowthBurgentBsorryByeahBmarksBgianviBelevatedBsoundsBpoundingBharvoniBironBwksB	retentionB
lisinoprilBcervicalBbuttBattemptBshakeBoriginalBneedleB
compulsiveBamountsB33B	wednesdayBsadnessBoccurredB	implantedBhumanBshouldntBwordBnineB
directionsBbingeBwideB	typicallyB	temporaryBnovBdrinksB	agitationBwreckBpracticallyB05mgBundetectableB	remissionBnowhereBhealedBcoolB52BweighingBsurpriseBsteadyBmaintenanceBknockBhysterectomyBbrandsBglucoseB	bupropionBplanningBincludeBbewareBaheadBweaningBremainedBguyBmaxaltBgrownBdramaticBdetoxBdecBaczoneBturningBsidesBringingB
protectionBnaproxenBmeantB	completedB10lbsBpcpB	necessaryB7thB42B37BpurposeB	negativesBlotionBinformedB72B600BthirstyBsodaBgradesB
eliminatedBcharmBtumorBmucinexB
continuingBankleBpopBbonesBalesseBaddingBtummyB800BpickedBstandingBocellaB	magnesiumBlupronBsoftBshakyBscaresB	psychoticBprayingBagonyBpropranololBlaidBineffectiveB	energeticBdecentBbusyB	potentialBlevaquinBeatenBbellyBaleveBthrilledBsupplementsBrestlessnessB	refreshedBmacrobidBeffortBboutBproteinB
instructedB	explainedBentirelyB	occasionsBmirtazapineBeffectivelyBattitudeBaspirinBanybodyBsadlyB	ovulatingBcostsBcontraceptionBseverityBparentsBnuvaBhepB	beautifulB250BusersBtermsBroughlyBimprovementsBhrB800mgBshakesBblameBstickingBsisterBdrunkB
developingBimpactBheroinBbenzosBresponseBcalorieBlbB
altogetherBworriesBwinterBneedlesBkillersBhelloBenjoyingBrefusedBplannedBlinerBhyperBzapsByoungerBsupportBpartsBnexiumBhopelessBflatB	disappearBblistersB95BvoicesBprovidedBflushingBdocsBrunsBprescribingBotezlaBnewsB	laxativesBgrowBbowelsB	augmentinBuserBtestingBstringsBpulseBnailBflexerilBcommentBspeakingB78BtabBpoisonBkillingBfunctioningBbottlesBleavesB
fluoxetineBclearsBchangerBcelebrexBcaughtBsciaticaB	insertingBpantyBfusionBfillBcouchBbalancedB
supplementBlumbarBfatherBfailureBdrysolBvenlafaxineBmethodsBkeyBfairB130B110BspokeBflagylByouveBwineButisB	tightnessBthoB	improvingBfewerBwalkedBrunnyBrockBportionsBchoicesBcheapBbeatsBspendingBpatienceBdriedBbattlingB54BrashesBlortabBlevoraBlaxativeBincidentBfaintBcautionBproperBmaximumBdependsBwetBreportedBobB	melatoninBlilettaBdollarsB	dependentBcolorB53BswearBmanufacturerBbuiltB
ultrasoundBfreakBpointsBminimumBfanBstonesBrespiratoryBoverlyBinducedBclinicalBamoxicillinBadmittedBendingB
continuousBbunchB	accordingBcatchBparticularlyBnoonBloweringBwroteBwateryBunbelievableBiiB
disgustingBvoiceBroadBremediesB
persistentB
metabolismBgameB
exhaustionBdurationBdullBcleanedBaddictBviolentBscarBreadingsBcorrectBcolitisBcodeineBresortBpelvicBlightlyBgentleB600mgBputsBoverwhelmingBirB400mgBzyrtecB	returningBmriBdoubledBdentistBcontactBtaperingBscarringBpregnanciesBeventsBandorB	amazinglyBukBteenB	preventedBpinchBconcernsBchemicalBbonusBvisitsB	screamingB
probioticsBperformanceBhorridBelavilBcomplicationsBprostateBpositionBhangoverBgynB	describedBcontinuouslyBbruiseBwiseBsuckedBrelapseBdosingBcombatB43BtoradolB	therapistBshiftBrestedB	purchasedBholdingBerectileB	disordersBdeviceBclindamycinBcardiologistB2007BtegretolBrheumatologistBrestroomBprescriptionsB
narcolepsyB58B47BtissueBratedBoutsBnightlyBhypertensionB	donrsquotB1200BswellBremainBpieceB4mgB180BreferredBmajorityBkyleenaBgutBcoverageBactBtvBshapeBresultedBlearnBvoltarenB	minastrinBmgdayBheatingBbecomesB	outbreaksBlessenBreceiveBnotedBliftB
interestedBglassesB510B	trazadoneB	stimulantB	probioticBpreventsBpowerfulB
optimisticBofferedBnumbingB10325BunitsBjumpBinfectedBairBvitaminsBsrBseptBinjectedBdiscountBboostBvacationBultramBthinksBdroppingBdadBcrampyBbotheredBbookBbelsomraBsinusesB
postpartumB
particularBmcgB	itchinessBthinningBsightBoccasionBclusterBwarnBsyrupBrelationshipsBdrawbackB	digestiveBachingByawningBstarsBlongestBjunkBformsBfocalinBcopperBtheyveBtaperedBmomentsB
forgettingBfixedBbreastfeedingBbreaksBsweatyBsplitB	risperdalBquotB
preventionBmillionB	increasesBfungusBembarrassedBduacB8thB810BzitsBsoberBshesBpeaceBlantusB
girlfriendB	eliminateBamitizaBsurgeonBrisksBquestionB	correctlyBcarbB20lbsB	twitchingB
stabilizedBrecallBjourneyBenjoyedBclotBpushedBointmentBfluidsB
determinedBconsiderablyBcasesBdiflucanBcrampedB51B3mgBreboundBportionBlethargyBjokeBbumpBbulgingB2000BrescueBfingerB	urologistBrelpaxBmobicBlengthBkillerBinvokanaBgynecologistB
tremendousBstenosisBpenBavailB	sinusitisBreducesBpostsBmucusBmotrinBlyingBlamotrigineBgildessB8mgBdaytimeBcrBbfB44BwouldveBsuckBsomehowBsavingBkidneysBflavorBdownfallBcoveringBbumpedB30sBtriggerBplayingBmildlyBharshBcellBadvairBtastingB	receivingBpumpB	immenselyBfibroBactionBreleasedBoctBlB	cognitiveB	radiationB	protectedB1015BshockBsettledBorderedBforthBdownsBcountingB	companiesB1010BwallBpureBladyBcopaxoneBappearBurgencyBgrossB
beforehandBtellsBpantsBimportantlyBdifferinBcalciumBbystolicBbayByayBunsureBsaltyBrapidlyBlowsBdripBcleaningB	alleviateB9thBovariesBinflamedBbydureonBunpredictableB
swallowingBskippedBshutB	resistantBremovingBpoopBpercentBhivBdosagesB
dehydratedBctBcompareB59B	urinatingBsneezingBrushedBodorBmetBknocksB	intrusiveB	happinessBflaresBfinalBcalmsBbuyingBbodiesByellowBunhappyBpowerBleadingBcrashBcheekBshowingBshinglesBpushingBprogressivelyBprogesteroneBhopedBbeerB39BtilBpeoplesB	heartbeatBfastingBdroveBdrasticB41BvsBteensBspeechBsortsB	resultingBresearchingBmedicareBlongtermBlaughB	infusionsBclassesBcardioB99B85BresolvedB	reluctantBplainBlaborBgreenB	destroyedB	chocolateB
trintellixBprovideBfactorBchampixB
bothersomeBtwentyBpinkBoutlookBmidnightB	lorazepamBkicksBfertileBeventB
decreasingBcenterBappearsBanytimeB	vengeanceBchickenBachyB375mgB
regardlessB
borderlineBusefulB	paraguardBlazyBintolerableBheresBeasedBdairyB06BsuffersBrollerB
milligramsB	forgetfulBallegraBswelledB
researchedBreglanBminocyclineBfortunatelyBwristBwhilstBtradeBspeedBrequipBrawB	psychosisBliftingB	exceptionB	consciousB	triggeredBstarterBbattleBrepeatB	positivesB
functionalBendsBendoBejaculationBbyettaB10thBsmallestBfavoriteB
didnrsquotB	worseningBoverwhelmedBlimitBdryingBdamnBurgesBsleeplessnessB	sleeplessBprofessionalBinvolvedBcoasterB1000mgBsubutexBspasmBlupusBgatoradeBepilepsyBeffectedBcvsB	childhoodBbringsB
blackheadsBatrialBalarmBstingingBlivesBinfoBdietingB	confirmedB	afterwardBwashingBusageBspringBrelievesB	reclipsenBlibriumBdogBdirectlyBdehydrationBunwantedBstairsBpartialB
metoprololBloosingB
irritatingB	cortisoneBcopdBbenicarBsometimeBsettleB	searchingBpriceyBphaseBlabBingredientsBindigestionBformingBfioricetBcampralBapptB30lbsB
suppressedBsuccessfullyBstabbingB	prolongedB	placementBnicotineBesteemBclockBbreezeB250mgBughBthighsBshirtsBpackageBoxyBdiscontinuingBblisterBaccompaniedBraiseBinsistedBfitsBdiscussBtubesBshameB	plaquenilBpapBmonitorBitchedBescitalopramBelbowsBeggBdiazepamBweatherBvimpatBstrictBruiningBjoyB	intensityBgastricBedBdruggedBdermBdensityBcurbBappearedB62BorangeBobviousBmiseryBmiraclesBjawlineB910BwoundB	somewhereB	regardingB
polycysticBinjectBdexilantBbathBassuredBassumeB
stabilizerBseriesBrespondBorsythiaBnsaidsBhardestBexistentBbarBwannaBviaBtortureBthumbsBstuffyBsedationB	hypercareB	everytimeBcalmingBblisoviBtravelBtingleB
noticeablyB
individualBinchesBgingerBfailBatriplaBvistarilBuncontrollablyBsluggishB	serotoninBsedatingB	relievingBpsaB
progressedB	phenerganBheldB
assistanceBappleB
adjustmentBtouchedBsaltBrelieverBpreBopenedBhydratedBgenericsBbreadBanemicBuglyBspecificallyBsodiumBshowsBrecommendationBperformBharmB	dexedrineB	attributeB160BusesB
ultimatelyBseekBrefuseBrealityBnatureBincreasinglyBconB	christmasB	singulairBrootBpredictableBlesionsBcitrateBcareerB2006BsolvedBrecoverBnervousnessBgarbageBendocrinologistBcontentBbenzodiazepinesBapparentBanemiaBthighBtaskBreplaceBintunivBbitterBbiteBbegunBbaclofenB140B10pmB	throbbingB
sensationsBraceBlialdaBbrotherBtemperatureBstatesBsizesBpossibilityBpacketBjittersBheadsBdrainedBachieveB4xB40sBsedatedBrehabBpubertyBformulaBfadeB	exercisedBconvenienceB	breakdownBblessBannoyedB	recurringBjanuviaBhalfwayBframeBnortriptylineBminiBmeaningBdecadesB
recoveringB
depressingBwartsBtipBshootingBpromiseBlaughingBholeBextentBexcessBdecadeBbenzoB
affordableBworsenButerineB	trileptalBstudiesBstrepBspecificBloudBlettingBflushesBcontractionsBarmpitsBstelaraBquickerBprilosecBintendedBfaultBcreatedBbusinessBauraBalrightBxareltoBwisdomBongoingBmetronidazoleBfruitBexamBballBtunnelBtriggersBnapsB
complainedBtrainingBtornBslippedB
parenthoodB
irrationalB
intestinalB	forgottenBexampleBbirthcontrolBxulaneBulcersBtachycardiaBsuffererBpropofolBfibrillationBdoxyB
comparisonB49BstretchB	outburstsB
moderatelyBfocusingBfifthBceasedBacceptBtremorBswingB	neuralgiaBdreadBanxietydepressionBallowingBwindowB
suggestionB	remainingBpowderBgallbladderBexcedrinBcrankyBweddingB	tretinoinBmosBflawlessBagoraphobiaBafibBshedBplansBhypothyroidismBhideBcherryBbattledBandrogelB2005B15lbsBzybanBtinnitusBthirstB
terrifyingBstingBsmellsBseasonalB	pulmonaryBopposedBluvoxBhighsBcrestorB
beneficialB8pmBsteadilyB	sclerosisBrushB
resistanceBrequiresBovaryBmirapexBgrandBevraBburstBbtwBawokeBaugB63B5lbsBwipesBtearBsuspectBsolodynBinterestingBfreshBfadedB	countlessBcomfortBclosedBbetaBasacolB5pmBuppingBshoppingBreliableBobeseBjumpedBedemaBaxironBwornBplasticBheckBdrinkerB
attributedBatenololBalliB125mgB
ulcerativeBticsB	slightestBrelaxersBpreventativeBpoppingBloopyBloloBkillsB
grogginessBdilantinBbypassBvarietyBthrushB	requestedBrelaxerBquietBovercomeBnursingBmaintainingBgrantedBfedBdeplinBbriefBwashedBtumorsBtenseBsBremindBpromisedB	potassiumBpainkillersBmoviprepB	medicatedBfullerBfibroidsBerrorBaidsB115BwavesBvisitedBtinglyBsurprisinglyBskinnyBrouteBrelyBmirrorBincontinenceBflushBdisabledBcautiousBantipsychoticsB	accidentsBspecialBlightheadednessBleveledBgroupBfolksBfianceBfetzimaBdepressionanxietyBattemptsBadministeredBosteoporosisBmiscarriageBlearningBhomeworkBfabulousB	encourageBemotionBconversationBchilledBbelievedBvanishedBvaBstepsBspacedBpatternBnopeBgastrointestinalBdiovanB
dermatitisB
dependencyB6pmB1520BsubsidesBspironolactoneBskippingB	questionsBporesBoccursBitchesBinactiveBexpenseBeuflexxaBdiscouragedB	decisionsBcomplexBblockedBbeatingBaccidentallyBtriglyceridesBtallB	stressingBroundsB
quetiapineBlevothyroxineBinternalBefficacyBblessedBxyzalBwkBthinnerB	symbicortBsurelyBspanBsnacksBliquidsBlabelBinhalersB	explosiveBdoorBcystitisBchairB7pmBtendsBstrainBstatinsBsnapBslowerB	psoriaticBpsBmetalBcocktailBbutransBbleedsBb12BassumedBantipsychoticB89BwrittenBvisualB	toleratedBremedyBremainsBovuleB	mononessaBhydroxyzineBdivorceBdissolveB61BzomigBwastedBulcerBtysabriBtipsBseparateBpracticeBlovesB
introducedBhassleBeuphoriaBdsBdentalBdecidingBdamagedBclottingB	alcoholicB3amB2004BzarahBveggiesBtestimBterrorsBsipB	realizingBoutcomeBimprovesBgreatestBgeneticBchancesBbackedB
applicatorB135BshrinkBpractitionerBouncesB
maintainedBhittingBhitsBgreasyBflonaseBfearsBcyclingBchatealBcetaphilBaspectB9pmBwipedBwaistBvisibleBrequireBpeelBlosartanBlevitraB	imbalanceBflightBcatB1xBwheezingBtableB	packagingB
outweighedBmalBldlBjolessaBgreaterBdiverticulitisB145BweightsBthirtyBretinaBlightsBfuzzyB
counteractBcoBbruisedB
appreciateB
alprazolamBteacherB
stimulantsBsanityBrodBrexultiBpurchaseBliningBeveningsBcapBcandyBbrutalB120mgBtopsBtemperB
peripheralBkarivaB	esophagusBelbowBeasiestBdelayedBclaritinBcheckingBcanadaB	attemptedB710BsportsBsmileBsitesB
ingredientBgoogleBdemerolB	delusionsBamongBzantacBwarningsBshirtBseasonBproviderBperoxideBopioidB
naltrexoneBmarkedBdilatedBdeskBdepressantsBdefiniteBconcentratingB	brilliantBblockerBavoidedBalternativesB	albuterolBweveBunmotivatedB
topiramateBsmartBsharingBsciaticBrisperidoneBplaqueBpatternsBnutsBmoBmddBmanagingBlipitorBinteractionsBhBfifteenB	clockworkBbagBavoidingBantianxietyB66ByogaBweekendsBwatchedB
unexpectedBtollBsavingsBproBpaperBopeningB	lightenedBlandedBinteractionBhenceBdepoproveraBcupsBcloserBblockBbirthdayBweenBunderstandingBsonsB
oncologistBmovantikBheaviestBgroceryBcontinBchemotherapyB170BxolairB	worthlessBwindB	underwearB
transplantB	stabilizeBribsBproduceB	proactiveBidkBheavenBfadingBenzymesB	energizedBcryselleBbuildingBtoenailBspaceBsoooBruinBrequestBprovidesBpoppedBopioidsBfishB	carefullyB	ambulanceBwildBwallsB	violentlyBstubbornB
sideeffectBpullingBpresentationB
prescribesB
overactiveBlabsBkickingBinjuriesB	contactedBbmsBbendBassumingBapneaB2030B
universityB
themselvesBsimponiBrubBrestingBoptedBofferBlinersBinvegaBguessingBfaintingBcellsBbsB900BthrownBsuggestionsB	primarilyBpradaxaB
penicillinB
miraculousBmassBinnerBbloatBappB64B2amBwhereasBupsideB	temazepamBteenageBstarvingBnobodyBlymphBhahaB	fortunateBflakyBessentiallyB	ejaculateBdeliveryBantihistamineBsnackingBschizophreniaBplayedBpageBl5BcloudB	chemistryB	buspironeBabscessBablationB4amBwipeBwinBunfortunateBtherapeuticBsoapBsipsBregimeBpsychologicalBpostingBphlegmBoxygenBhimselfBgumsBflushedBfallsBcuriousBbloodyB
amlodipineBwhosB	vaginitisBtowardBtitratedBskyBreactedBpharmaceuticalB
paroxetineBherpesB	clenchingB6amB
whiteheadsBvergeBtoneB	suspectedB	persistedBintermittentB
heightenedBfridgeBfaithB
diclofenacB
complexionBbothersBaleB98BtemporarilyBsoughtBpsychiatricBlistenedB	lidocaineBfallenBbphBwithdrawBunknownB
transitionBtoolBskelaxinB	reviewersBretiredB
rememberedBproducedB
perscribedBmediumBidealBfaintedBeuphoricB	diarrhoeaBcottonBcostlyBcomputerBbrushBwristsBwoBtownBspirivaBrollBproudBmeetBcrawlingBcopingBworkerBsoldBseniorBscansBpinchedBoutquotBlumpsBlaserBdoxepinB
disabilityBdegreesB	commentedBbearB
autoimmuneBaubraBanxietypanicB12thBwatchersBpenisBpassesBpaB	operationBmenstruationBmenB	meloxicamBjobsBinderalB	impulsiveBexplodeBexcessivelyBcountryBcleanserBclarityB	chlamydiaBburpingB
anesthesiaBzolpidemBwokenBtwelveBsuppositoryBsilenorBreachingBpeeingBpanickedBmileBmeatBlifetimeBgallonBcipralexB	blemishesB175BrubbishB
remarkableBpremarinBmlBhubbyBenlargedBdreadfulB	daughtersBchappedBbaseBacetaminophenB15thB11thBviberziB	profuselyBhappilyBgoogledB7amBwarfarinBtoenailsB
supposedlyBstandardBindeedBgottaBgagBforumBdownhillBcrappyB230B
trigeminalBtiedBsumatriptanBstribildBsizedBordersB
officiallyBnumbedBmeasureBenemyBdriesBdrawBdeeplyBconceiveBboardBbmiBbenignBtroublesBtoxicBsurfaceBshrunkBseekingBsedativeB
productionB	lingeringB	injectingBgrapeBgeneressBfactorsBeaterBbreoB911BunstableBsubtleBridiculouslyBprotonixBoutgoingBexpressBdisorientedBdisasterBcontributedBconsultBclueBbugBadhesiveBversusB	travelingBtadBscratchB	recoveredBragingBpermanentlyBpancreatitisBnursesBmylanBibsdBheightBfrustrationBfattyB
equivalentB	endoscopyB
encouragedBdifficultiesB	determineBchillBcallsBbringingBblowBbikeB1530BwardB	walgreensBvaryBtrashBtimingBtendonBsuppositoriesBscoreBpsychologistBfoughtB	essentialBazithromycinBataraxBaaBsynviscBstickyBpukingBprovedBpennyBloadsBlayerBinterstitialBinflammatoryBgassyB	committedBcbtBbeginsB1500B1012BtouchingBsooooBscreamBrestasisBreligiouslyBrelapsedBreB
proceduresBpotentBphobiaBnecessarilyBinchBholyBformerB
disturbingBcrackedBbeautifullyB3pmB225BzanaflexBunwellBtraumaBtempBsuppressB	substanceBrupturedB
reasonableB
monitoringB
menopausalBlowersBjumpingB
interferonBimplantationBhundredsBhatBflaredB	downsidesBcompleraBcheckupBbowlBbitesBbenzoylBarrivedB17thBzianaBwhoeverBurBstreamBrestorilBrestfulBprepareBpiecesBnorvascBnoiseBminimizeBmattersBhyperhidrosisB	genuinelyBencounteredBdiskB
dependenceBcrossBcakeBbraBbiopsyBbareBaddressB10xBzubsolvBswimmingBstreetB	reccomendBqueasyBprovenBpickingB
negativelyBlordBhookedBforteoBflulikeBflashBevilB	discussedB	australiaBapprehensiveB	welbutrinBultraB	sufferersBsociallyBsmilingBslipB	secondaryBsearchBsaferBmilitaryBmeetingsBignoreBgastroBfailingBegBdebatingBcloggedBcarriedBwiredBtshBspinningBresponsibleBquarterBportiaB
pleasantlyB	panickingBlightenB
depressantBclomidBbizarreBaveloxBagesB8amB4pmB11pmB01BxiidraB	verapamilB	treadmillBstyleBretinB
regulatingBprevacidBplateB
physiciansBlessonB	graduatedBgaggingBcontraceptivesBcontinuallyBboysBboomBautoB5amB2001BwalmartBvolumeBucBtrialsBtargetBsurgicalBsoundedBrinseBplateauB
outrageousB	nighttimeBmonsterBmBinteractB	indicatedBcutsBchargeBblahB450B225mgBvlBtreeBtowelBsubstantialBsronyxBsheddingBreluctantlyBpalmsBovulatedBnodesBlikesBimpactedBconversationsBconsumedBadB210BwoodB	titrationBteamBsoftenerBshownBrhythmBquottheB
pharmaciesBpetiteBnicelyBmumB
meditationBiudsBholidaysB	heavinessBgradualBeBdietsBbeggedBavonexBwakesBundiagnosedB
underlyingBresolveBraisingBpinsBlilBlamisilBinconvenientBimodiumBfruitsBdreadedB	congestedBcloselyBchewBazorBanswersB930B290B24hrsB025B
vegetablesBtmjBskillsBquothighquotBprotectBpolarBpanickyBnewerBmisdiagnosedBkitchenBhctBgilenyaBfreedomBcuzBcurbedBcourageBcarryingBbentylB
aggressionB	addictingB69B350B12mgBxrayBvesicareBupdatedBsmootherB
scratchingB	scoliosisBruleBmixtureBlymeB	lesseningBintimateB
intestinesBgallBfiorinalBfearfulBdotBdiffBculpritBcommitBboatB
alleviatedB40lbsB2pmBxraysBvivitrolBtextureBslowingBsettingBsafetyBrelateB	recognizeBnipplesBminusBloseasoniqueBlatestBlaBitquotBinjuredBignoredBexternalBcountsBcoumadinBcalfBapprovalBzoneBveinsBtruckBtrainBtapeBt3BstoresBprivateBorgasmsBlikingBicBhopelessnessB
governmentBfitnessBfearedBfavorBentB	decreasesBcruiseBcheeseBcanalBbiaxinBadditionallyB700B2002B09BxyremBspikeBpaysBnpBmthsBhospitalizationBhillBgroinBgladlyBflareupsBenemaBeggsBcordB
contractedBcomfortablyBcaresBbcpBbatchB
acceptableB79B240B105BzitByearlyBwishedB	unhealthyBtreximetBtracksB	spreadingBslowsBshoesBrepeatedBrefillsBrapafloBpreparationBmaskBinexpensiveB	gastritisB	functionsBelmironBectBdaytranaBcontainsBcardiacBbiB	benzaclinBbamBapproveB	alternateB	affectiveBabusedB30thB2xdayBvagifemBtirosintBsuppressionBstagesBsourceBripBresidualBrelaxingBneurologicalBneuroBmeetingBfrighteningBfeedbackB
concerningB	bedriddenBabortionB2003ByellingByallB	tremblingBtreatsBsourBsooBshallBrobaxinBpuffyBprepopikB	interfereBherniaBgrumpyBfreshmanB	coworkersB
aggravatedBacupunctureBtemperedBsubstantiallyBstungBstringBshyBpukeBpleasureBplaguedBmovieBlemonadeB
horrifyingBherbalBflareupB	enjoyableBdistressBdelayBcoupledBapproachB	allergistB7lbsB25lbsB025mgBtolerantBthanksgivingBsunburnBspecialistsBsolveBsmellingBlumpBleadsBhangingBelectricBdoryxBdiminishB
counselingB	consumingBconsumeBbucksB	botheringBbasicBabruptlyBworkoutsB	urticariaBstockBstentBspraysBsmearBreturnsB	restartedBrebifBpaceBhairsBgenvoyaBfractureBforgetfulnessBfittedBdespairBcloudyBbandBapplicationsBachedBuncontrolledBtriumeqBsubsBsillyB	seeminglyBsaladB	recurrentBquotiB	producingBorallyBobesityB	monitoredBmoisturiserBmobilityBmessingB
idiopathicBhrtBholidayBflavoredBdisappointingBcouldveBcontactsBbetBawkwardBatypicalBadjustsBacceptedB750B50lbsBsoakedBparnateBlimbsBirritateBimuranBhyperplasiaBhdlBflakingBenduredBendlessBcrystalBcrackersB
connectionBcoilBclimaxB
backgroundBathleteB19thB165BwebBstoodB	sparinglyBsoakingBsheetsBreportsBpurpleBprepsBpremenstrualBpelvisBmachineBlorynaBleanBkeflexBkadianB	jardianceBideationBhusbandsBheavingB	fracturedBformedB	extensiveBexistB	exercisesBensureBdrainB	connectedBcombinationsBcircumstancesBcaneBbutterBblockersB	behaviorsBbaldB16thBwonderedBwickedBunderweightBtearingBspellBsharedBplavixBlivaloBlawBkgBjudgeBinsidesBfieldBellaBdreadingB	dramamineBconsiderableB
commercialBclaravisB	challengeBbedroomB	acyclovirBabusingB90sB90mgB28thBtrappedB
substituteB
subsequentBproofBpentasaB	outweighsB
omeprazoleBoBlovelyBjanumetBideasBfinaceaBfalseBdumbBdisksBdetailsBdeclineBcurbsBcreditBchoresBbumpyBbrighterB
advertisedBactosB80sB50sB220BvraylarBvagB	tamoxifenBslimBshitBregionBregainBrangingB	paralysisBnardilBmixingBgroundBgramsBglutenB
eliminatesBdiureticBcoincidenceBcatheterBblocksBbesideBbenzodiazepineBantihistaminesBagedB5325B20thBzoviaBwalksBvaryingBtumsBtruthB	sunscreenB	subsidingB	shortenedBrnBribBpillowB	performedBpathBlitreBjuniorBinsanelyBinconsistentBicuBhclBgrindingBfreezingB
foundationBexBdreamingBdefBcumBcoregBcaringBboundB
aftertasteBactsB830B1pmBzpackBweenedBunmanageableB	underarmsB	thousandsBthickerBstaredB	soolantraBrollingBriseBresumeBregardsB	poisoningBpizzaBperiodicallyBpeakB
olanzapineBmuscularBmembersBmakersBhysinglaBhungBhandfulBextremitiesBdiclegisBcrisisBchewingBbodysBbidBashamedBappointmentsB	advantageB6mgBsuppressantBsulfateBstarBsobbingBschizoaffectiveBrelaxesBregainedBparkB
orthopedicB	occurringBnhsBmodeBmemberBmassageBlinkedBfoulBemotionlessB
distractedBdesperatelyBbpdBalternatingB	agonizingB14thB	unrelatedBunexplainedBuncommonBtwitchesB	syntheticB
sufficientBseverBrealisedBreactingBnauseasBleakageBjubliaBfemaraB	estradiolBdrippingB
delusionalBcoconutB	chemicalsBbrothBbeggingBalteredB8lbsB730B190BwheelBwelcomeBvileBvaselineBunitB
stretchingB	softenersBpotentiallyBoraceaBibscBhidingB	glipizideBfilmBeyebrowsBdvtB	cranberryBcookBconfirmB
completingBclothingB	bilateralB	arthriticB
accomplishB9amBwhackBweltsBtypingBsessionBsemesterBpoorlyB	paralyzedBmanufacturersBlactoseBintendB
hashimotosBgolfBgenitalB	fertilityBeyelidsBexposedBerraticBearthBconvinceBathleticBwitsB	untreatedBunrealBunbelievablyBtendonsBt4BsoundlyBshopB	shakinessBsafelyBpraiseBintensifiedBgramBgelsBgastroparesisBgastroenterologistBfollowupBevenedB
departmentBdeniedB
contributeBaldaraBafBabsorbedB10mgsBtwentiesBtraditionalBtossingBthreateningB
thoroughlyBsworeBsulfaBstainsBrunnerBriddenBrecordBpupilsBpuffsBpigBovulateBlegalB	knowledgeBhydrochlorothiazideBeliminatingBdressBdopamineBdislikeBdevastatingBdependBbusB750mgB16ozBweakerBwaryBuricBtennisB
suppressesBspondylitisBsectionBrhinitisBreverseBretainBpumpsBpartumBnoisesBlooBfistBfakeBexistingBenvironmentBdevilB
devastatedBcuresBcrushedBcreateBconsequencesB
conclusionBassistBacanyaB20mgsB18thByoungestBwishingB
wheelchairBtoreBtoprolB
tendenciesBstaringBrippedBresumedB	respondedBrepairBrBpartyBinventedBhyperactivityBhallucinatingBfamiliarBdrainageBcornerBcoldsBcleanseBbumBbrownishB60sB10amBx2BvisitingBunderarmB
thereafterBtaytullaB	talkativeBstingsBstatingBsquareBsporadicBspikesBsearchedBrevealedBremindedBpuffBpharmaBpdocBmrsaB	modafinilBlesserB	influenzaBincludesB	impatientB
imaginableB	hypomaniaBfibroidBendureBdutyB
dischargedBcravedBcoloredBchemistBbuttonBbulletBbegBauditoryB
anestheticB13thBwaveBstuffedBsticksBsnappyBrefusesBpyloriB	progestinBpoopingBplaquesBnormBmellowB	measuringBmagnesiaBkitBheadingBfranklyBexacerbatedBedgyB	deodorantBdementiaBcpapB	canrsquotBburntB74BzB	willpowerB	upsettingBtransfusionBsurviveBsaneBrotatorBrespondsB	relieversBmusicBmatchBfriedBforeignBfittingBfarxigaB	directionBdiagnoseB
ankylosingBachievedBabsorbB330B3045B2yrsB	withdrawlBweeningBvinegarBtwinsBtripleB
tendonitisBstimulationB	splittingBsoooooBslippingBscriptsBscrewsB
recurrenceBrealiseB	quotbrainBpeacefulB
parkinsonsB	ointmentsBmessyBmarathonBlinesB
healthcareBcontainBchallengingBbursitisBblindB
appearanceBannualBakaBtopicalsB	titratingBthumbBterrificBtenuateBstatinBshocksBserviceBrespectBpooBpollenBpharmacistsBnewlyBnamesBmirvasoBlubricationB	illnessesBhaldolBflyingBdystoniaBdistanceBdesignedBdepotBdarvocetBcultureBcreepingBclosestBclaimB
cellulitisBbleedingspottingBbenedrylBbeachBautomaticallyB	anafranilBaccurateB1997BvariesBtrainerBteachersB	strugglesBsixthBseldomBpurseBproliaBpinchingBpictureBmidolBfirmBepiduralBdesperationBcookingBconsistencyBblewB	averagingBaccessBwithdrawingBupquotB
understoodB	stabilityBstBspouseB
reasonablyBpolypsB	partiallyBpaleBmilderBmigranesBmidrinBmalaiseBlifelongBlastlyBl5s1BhavenrsquotBexposureBeasesBdozenBdermatologistsBcuffBcomplicatedBcokeBclimbingBbannedB
antinauseaBanticipatedB71BwashesB	vaginosisBtendencyBstadolB	shorttermBs1B
roxicodoneBpitsB	percocetsBnostrilBliceBleakingBkiddingB	jolivetteBjeansBintoleranceBhydromorphoneBglowingB	fracturesBfianceacuteBfacilityBexistsBestraceBdozensBdesiredBcouldnrsquotBcolonoscopiesBclarithromycinBadvancedBadministrationB	adapaleneB1030B	zithromaxB	wellbeingBvotrientBtriesB	solutionsBrocephinB
repeatedlyBpainfreeBoverthecounterB	orthoviscBlockedB	listeningBlevemirBindomethacinB	hepatitisBgurglingBgleevecBfeedingBexplanationBdirectB	compazineBcombineBchuggedBbuprenorphineBaspectsB70mgB18mgB
surgicallyB	retainingBreoccurringB
recommendsBpromethazineBplacebosBphBpeaB
overeatingBneuropathicB
miconazoleB	marijuanaBincomeBimmenseB	horrifiedBgBflyB	financialBexpectationsBdarkerBcsectionBcreatingBcoughedB	continualB27thB25thBwhoseBwastingBwalkerBupsetsBstaffBstabilizersBspaceyBsomebodyBshaveBshampooB	providingBprnBpresentationsB
pregabalinBoverdoseBounceB
occurrenceBmicronorB	meclizineBlyzaBlackingBinsBhungoverBgraduateBfyiBforteBdulledBdragBdisturbancesB
discussingBcrackBconsumptionBcapsBcalvesBblockageBb4BarimidexBanginaBaciphexB8ozB530B185BunsuccessfulBtoastBsurvivedBsuperiorBstumbledBsoftenBshiftsB	seasonaleBseaBrubbingBrenalBrecommendingBnataziaBmotorB
moisturizeBmoistureBhesitateBecstaticBdrixoralBdisconnectedB	deliveredBdeeperBdatingBcustomerBconsecutiveBchartsB
carbonatedBboilsBbelchingBappreciatedBaccountB45mgB26thB1amBtresibaBsuitBstaphBscariestBscalesB	requiringBrecalledBprogressionBpicturesB	painfullyBnitrofurantoinBmakenaBlogB	inhibitorBhiveBhemorrhoidsBhaBftBfeedBfBeuropeBedgesB	disgustedBdifferencesB	consultedBcomaBclaimsBblowingB
adrenalineB3xdayB3040B1020B
underneathBspiralB	speciallyB	shrinkingBsessionsBrestoredB
respondingBrelaxantBpsychiatristsBproveB
phenomenalBpeeledBorenciaBnearingBnaggingBletterBinformBhypothyroidBfenceB
faithfullyBfacedBcrawlBclosingBchartBcentralBcatchingBbuzzBbulimiaBbillBbileBatralinBanorexiaB	alertnessBactionsB
accustomedB610BveinB	swallowedBstudyingB	socializeBsnriBsmelledBskyrocketedBsickerBscaringBsaviorBsailingBrisingBrelapsesB	prolactinB
projectileB
positivelyBpmsingBmucousBmotionsB	milligramBmeiBlocationBl4BinsecureBgoldBfrozenBexalgoBepclusaB
deficiencyB
deductibleBcouponsB	cleansingBciderBchewableB	cessationB
cephalexinBcamreseBbruisesBboxesBbonineBantiinflammatoryB	alongsideBafteraB29thBupbeatBunluckyBunexpectedlyBtriprevifemBtrickyBsystemicBspottyBspontaneousBsofterBsodasBsocietyBscrewedBremBqvarBprostatitisBprofoundBproducesBpersistBpensBpediatricianBouchBoilsBmrisBmailBlucidBlaughedBitiBforumsBfolicBflippedBfillingBemptyingB	compelledBcigsBbpmB
behavioralBavaproBassBantabuseB73B3lbsB21stB212B155BxenicalBtriptansB	sustainedBstentsBstatusBshortenBrituxanBpyleraBprotocolBpoopedBparentBpanicanxietyBopportunityBopBoffersBnotableB	nonsmokerBneconBnauseavomitingBmeniscusBmeasuredBmagicalBlysineBliterBleaseBlaundryB
intolerantBinconvenienceBhungrierB	hospitalsBhorseBheelBgrabBgenotypeBfulltimeBfullnessBfrovaBeliquisBdogsBdeclinedBchoosingB	cataplexyBcabinetBblockingBazoBarmyBaravaBampyraBaccomplishedByeaByawnByardBwomensB	withdrawnBwearyBtheydBsliceBsetsBserveBsennaBrsdBprojectsBovereatBoddlyBlystedaBlevelledBlatterBhiccupsB
headednessBhappiestBhandyB	guaranteeBfeversBdiamoxBcreativeBchokingBbudsBbrieflyB
attemptingBappropriateB	akathisiaB22ndB102BvodkaBunisomBtwitchBtrustedBtikosynBswimB	showeringBshouldveBshavingBrollercoasterBreminderBrefilledBqysmiaB	positionsB	obsessingBnsaidBnasonexBmoviesBmoonBmintBmeltBlibraxBkilosBinstanceB	inpatientBguaifenesinBgamesBforcingBfeverishB
exhaustingB
duloxetineBdiaperBcomplimentsB	combiningBcoatBcheatBblamedBanyhowBadderalB88B6lbsB630B54quotB1dayB112BzpakBzohydroByuckyByuckB
withdrawlsBwarBvalveBtmiBthBtessalonBsubB	strongestBsolelyBsnrisBsellB
selfesteemBsalineBrozeremBrestartB	responsesBreplacementsBreflexB
recognizedBpurposesBpurgeBptBpressBprecumBpopularBpokingBperspectiveBpairB
painkillerBpBoutstandingB	organizedBnovologBniteBinvoluntaryB
interferedB	insomniacBhayB	fogginessBenemasBembolismBelsesBdiscolorationBcreatesBcounsellingBcoordinationBclumpsBceraveBbathsBaurasB
administerBaddictsBabsenceB2000mgBzincByoudBtitrateBthinnedBstonedBstigmaB	sophomoreBsmoothlyBshelfBsemenBresidueB
relaxationBrationalBpustulesBproblematicBprediabeticBpotBpissedBpeanutBobjectBmistakesBjailB	intenselyB
impairmentBillegalBglandsB
flecainideBequalBenormousBdiseasesBcontemplatingB
consultingBconnectBburpsBbelieverBatleastB
antisocialBalarmingBafrinBactedB875B23rdBviewBvButterlyBuniqueBuloricB	thresholdBtechnicallyBtazoracB
pronouncedBpoolBparacetamolB
outpatientBnonhormonalBnonethelessBmessageB
medicatingBlimitsBlasixBknotsBknifeBhoneyBgutsBgoshBglaucomaBgiftBespB	effectingBdxBdrivenBdangerouslyBchoreBcancelBcalmnessBbouncingBbossBblankBbeliefBbackacheBattendB	anxietiesBadvanceBadultsB24thBwheatB	vertebraeBunnecessaryBtorsoBsugaryBstaminaBstabilizingBsomeonesBsneezeBscienceBridingBprogramsBproactivBppBoatmealBnuisanceB	nebulizerBmelasmaBleakBjerkingBharmfulBflaringBfiredBdisappearingBdeprivedBcrashedBcozaarBcontributingB
consultantBconcentratedBbunavailBawaitingBarticleBallopurinolB70sB3xsB325B1998B1100B103B005BwipingBwhomBwedBvariedBtubB
tighteningBsunlightBsemiBraynaudsBquantityBpeBneutralBmscontinBmistBmetrogelBisolatedB
infrequentBhypoBgulpsBgrabbedBgiantBflipBfightsBexforgeBekgBectopicBdressedBdrenchedBdmB
disappearsB	digestionBdenialBdelsymBddB	counselorB	containerBchiropractorBcapableBcalendarBbitsBbitingB	beveragesBbananaBadjustmentsB
absorptionB
wasnrsquotBventolinB	unusuallyBunawareBtonsilsBsuppressingBsulfasalazineBspermBsmokesBshowersBshockingB	shiveringBshellBservedBseatB	receptorsBprofuseBprayersBpouredBpitBphenBpfizerBpersistsBparticipateB
pantylinerBobsessedBmessesBlimpBlifesBlargelyBjoinedBhumalogBhostileBhelplessBhavocBgroupsBfellowBdollarB
dissipatedBdangerBcrackingB	candidateBbuzzingBbillsBbeholdBbandaidBavinzaBautismB	appearingBappealBachillesB57quotB56quotB1500mgBwrinklesBwrapBviciousB	tussionexBtoddlerB
sublingualB	strainingBsteppedBsqueezeBsoothingBslurredBsituationalBsecretBscabBsatisfactionBrocksBriceBrectalB	presentlyB
podiatristB	plateauedBplaneBpeelsBobsessBnodulesBneverthelessBmunchiesBmicardisBlevoxylBhatingBgtBgriefBfollowsBexcuseB	excerciseBenterBdriverBdrainingBdoubtsBcurledBcrippledBcoursesBcolorsBballoonBauntB86B84B4lbsB1999B08BweepyBvyvanceBusaBuneasyBstressesB	strangelyBsoakBsingularBshortsBreviewedBregardB	proceededB	prematureBpplB
percentageBpacketsBoweBorganicB
operationsBnumbsB	noticableB
negativityBmiddayBmedicateBlessensBjumpyBivyB
inhibitorsBimmunityBgileadB
flatulenceBfacingBexpiredB
experimentBducolaxBdopeyBdetectedBcoworkerBclimbBclampBcdB	cartilageBcarbamazepineBcalBboredBbeersBawakenB55quotB430B3dayB36mgBwoozyBwishesB
weightlossBveteranBtossBterrorBtapB
tamsulosinBstrictlyBsleeperBsafyralB
restaurantB	relapsingB
propertiesB	preferredBpotencyBperkBpantiesBopinionsBonoffBmisoprostolB
literatureB	instancesBimpairedBimaginedBhyperactiveBhellishBhackingBfoamBfluconazoleB
electricalBeasingBdownwardBdoomBdoesnrsquotBdiahrreaBdetailBcreepBclustersBclammyBbummerBbuildsBbatBbagsB
artificialBarteryBantsBanalB52quotB150lbsByellB	vomittingB
vicoprofenBvastBtevaBterbinafineBtarcevaBsymtomsBsubsequentlyBsnappingBsinemetBscopeBroleB	ribavirinB	remainderBquotohBpersonsBperiodicBpangsBordinaryB
moodswingsBmemoriesB	medicallyBmajorlyBlengthyBitemsBitdBhealsBhbpBgooglingBgoalsBfriendlyBexamsBenabledBefudexB	duragesicBdisorientationBdextroamphetamineBconjunctivitisBcompoundB
comparableBcleanerBclaimedBchosenB	chapstickBcdiffBbridgeBboosterBbiggieBbelievesBbelbucaBaquaphorB	annoyanceBanaphylacticBadequateB3yrsB2weeksB13lbsB108BzoviraxBzelnormByaBwortBtradeoffB
supportiveBsuitsBspiroBsoupBsobrietyB	sentencesBscrewBrolledBpreexistingBpillquotBnoticesBmothersBmannerBlittlestBlifechangingBleukemiaBl4l5BknockingBjelloBiu2019mBimplantsBhintBfortyBformulationBfootballBeyesightBdymistaBdotsB	disturbedBdestroyBdesignBdddBcontrollableB	characterBcerealBceftinBcamperBbendingB	arthrotecBalbeitB2xsB10000BxifaxanB	worrisomeBtroublesomeBtobaccoBsulfurBspursBspokenBspiteBspicyBsonataBsmokersBroseBrecommendationsB	processedBproairB	prefilledBposterB	plummetedBpamphletBorgansB	nutritionBniacinBmoodierBlozengesBlandBinterferingBhalflifeBhairlineBgpaBfoldBfluctuationsBfeaturesBfailsBevidenceBenhancedBengagedBembarrassmentBdnaBdietaryBdetoxingBdetachedBcellceptBcaredBbentBballsBafrezzaB9lbsB53quotB12lbsB1200mgB100lbsBwreckedBversedBtrackingB
thatrsquosB
tendinitisBskullBshootBseventhBsentenceBsandwichBsaladsB	reportingBprintBplantarBpinBpharmaceuticalsBpeptoBinvasiveB	intensiveBfirstlyBfatalBeffortsBdustBduleraBdishesBdisappointmentB	diagnosesBdetrolBcreptBcosentyxBcorrelationB	containedBcompressionBclassicBcarpalBbooksBbackingBawakenedBattachedBarriveB	apartmentBanxiousnessBamericanB
alzheimersBagingBacheyB11amBzetiaByepBwartBvaccineBupcomingBunlikelyBunderstatementBtransfusionsBtoxicityBtnBticBsweatedBsupperBstrokesBspareBsleepsBscarredB
regulationBquartersB	presentedB
precautionBocularBnrBnotesBnaturethroidBmethylphenidateBlotionsB
ironicallyB	intestineB	intervalsBinterruptedBinputB	houseworkBhikingBhazeBgulpBguiltyBgrandmotherBexplodedBenteredB
definatelyBdanceBcousinB
concussionBcommonlyBclobexBciprofloxacinBcathBbuttocksBbiologicBarmpitBadaptBabsentB82B5xB5ftB50mgsB3monthsB1130B10yrsB100sBzipsorBwatsonBwackBvivelleBvalueB	unsightlyBthoracicB
stuffinessBsnappedBsakeBrumblingB
roaccutaneBrepathaB
relentlessB	regulatesBregimentBranexaBpunchB	promisingBprenatalBprayedBorthotricyclenBorlistatB
neutrogenaBmindfulBmindedB	metamucilBisotretinoinBirsquodBintractableBinstabilityBinduceBibranceB	hypomanicBhyperpigmentationBhandledBgeneralisedBgapB
frightenedB	fluctuateBendometrialB	efficientBcrossingBbubblyB	behaviourBbaselineB	backachesB
arrhythmiaBarousedB	addressedB83B76B3monthB3000B130lbsB104B101BzapsquotBxsBwomansBveganBtransformedBthousandB
techniquesBteaspoonBsystemsBsyringeBsouthBsociableBsexyBsendingBsecondlyBsalesBsafestBrushingBruptureBrubbedB	replacingBrenewedBrampedBquotperiodquotBquotitBquestioningBpurgingBprotopicBprobBpricedBpalmBouterBoptBmultivitaminB	minimizedBjohnsBirsquollBindustryBhypotensionBherselfBhatesBhandedBgrowsBgrandsonBgraceBexceptionallyB	estaryllaBenteringB	dissolvesB	disruptedBdiscontinuationB	disablingB
destroyingBdepersonalizationBdeadlyBcurveBcurlBcrushingB	confusingB
childbirthBcefdinirBcategoryBbreathsBblackedBbasalBassureB
afternoonsBaddsB215B2025ByoursBversionsBunacceptableB	thorazineBtheoryBtaltzBsusceptibleBsurvivorB
surprisingBsupervisionB
suggestingBsudafedBspeculumBskliceBshelvesBsexdriveBsavesBrocketedBremindsB	referenceBreferB	radiatingBpredictBpassagesBonextonBoffsetBmindsBmeantimeBmagBlobeB
insatiableBinquotBholdsBhallucinateBgfBgeneBfearingBeatsBdrugquotBdrivesBdiscoverBdimeBdilateB	demandingBdareBdancingBconsiderationB
compoundedBcommercialsB	cancelledBboostedBblemishB	aspergersBapproachingB	apatheticBalterB
alcoholismB450mgB1992BvulvaBtubalBtheyllBsinkBserumBsalivaB	salicylicBreviewerBreliedBrecomendBreactiveB
purchasingB
preventiveBpointedBplaysBplacingBpassionBpancreasBoxysBmomsBmildmoderateB
mentioningBmedicaidB	magicallyBlomotilBliteBlapBhugelyBhersB
hereditaryBhepcBgentlyBgaggedBfusedBfoolB
explainingBequallyB
dyskinesiaB	dismissedBdarnB
contactingBchurchBbyeBbonivaB	biologicsBbacksB	associateBapathyB
angioedemaBacB900mgBvideoBtriptanBtoviazB
toleratingBtetracyclineBtekturnaBtearfulBsystolicBshoweredBretailBresponsibilityBresistBquotnoBprazosinBpairedBobtainBmtxBmishapBmetadateBmakerB	lubricantBlousyBlimitingBguiltBgaspingBgabaBfleetBfastinBerrandsB	dysthymiaB	dysphoricBdoableBcoughsB	componentBchugB	childrensBchewedBchaseBbudgeB
blisteringBbcsBbackupB	argumentsBaloeBaintBagreesBagentB60lbsB410B1996B118B100xB07ByikesBvesselsB
undetectedBtrackerB	toothacheB	therapiesBtemptedB	tecfideraBstormBspottingbleedingBsortedBsjogrensBsettlesBsecurityBscreamedBrulesBrockyBrepBrectumBrearBoverseasBmiraculouslyBloadingBishB
internallyBinsistBhundredBhonestyBglandBfungalBfetalBembedaBelevenBditchB
discourageB	diltiazemBdeserveBcriticalBcoolingBcontraryB	consistedBcommunicateB	collapsedBcnsB
clonazapamBcityBcheatingBceaseB	carcinomaBburstsBbrittleB	believingBavodartBargueBadsBadipexpB96B160mgBwtBwrappedBsuckingBsteerBspikedBspectrumBsixteenBsettlingBruinsBrobotBriskyBresponsiblyB
requestingBrenewB	relaxantsBregretsBrefrigeratorB	realisticBquotsideB
propanololBprevifemBpetB
perceptionBpdsBouttaBmotivateBlitersBlevofloxacinBkenalogBimpulseBidiotBhypersomniaBhydrocortisoneBhydrateBhomeopathicBguineaB
fluctuatedB
extractionBexertionBerectBelidelBefficientlyBeducatedB	donu2019tBdiminishingBdescriptionBdegenerationBdearBcuteBcrashingBcfsBcapacityBbrushingB
breakdownsBbontrilB	bloodworkBbananasB	awarenessBatrophyB	asthmaticB	assistantBapplicatorsBacidophilusB	abilitiesB12080BzonegranBurethraB	underwentBtemplesB
suspensionBsubjectBstripsB	straterraBspitBsmellyB	scepticalBruledBreversedBresetB
proscribedBpromptlyBpromptedBpoisonedB	pointlessB
peppermintBnubainB	nocturnalBneuproBlockBlinkBlimeB	involvingBintermittentlyB
injectableBherbsBheelsBfusionsBflakesBfixesBfingernailsBfiguringBexistedBdrierBdoublingBdivorcedBdirtyB	dependantB	delightedBdandruffBdamagingBchipsBchecksB
challengesBcarriesBcanadianBbaseballBamphetamineBaddadhdB	achievingB6monthsB205B200lbsB1800B1400B1014B100mgsBvehicleB	vaginallyBupdatesB	trimesterBstupidlyBspfBsomedayBskinsBsilentBrxdB
ruminatingB
remarkablyBrelyingBrefusingB	reassuredBpvcsBpeedBoptimalBoffquotB
nosebleedsBnorcosBneurologistsBmathB
impressiveB	householdBhideousBguardBgripBfortuneB	formularyBfloridaBfloatingBfemurB	existenceBexaggeratingBeveBemsamBdetoxedBdeterioratedBdesogenBcuringB
creativityB	completlyBcoatingBcoatedBchronicallyBcashBbugsBbrainsBbombBbarrettsBaubagioBathletesBaptBanusBailmentsB5mgsB310B200sB195B178B0025BundesirableBultracetBtrilumaB
travellingBtopomaxBtiniestBtinglesBthoroughBtardiveBtankBsutentBsuggestsBstudiedBsternumBsocializingBsistersBsimplestB	scatteredBropeBrestrictionsBqBpusBpsychoBprolapseBprayerBpartlyBordealBnorethindroneBmonodoxBmoisturizingBjuvenileBfreelyBforearmsBfactsB	enjoymentBeloconBdrawnB	dizzynessB	diastolicBdeprivationBdarknessBcornersBcontractionB	conceivedB	communityBchasedBchalkedBburdenBbeautyB
antibodiesBalteringB91B275B12hrsB1214BunsteadyBunitedBunderstandsBtowelsB	tourettesBtoujeoBthinnersBteachBtalksBtackleB
synvisconeBstdBstabbedBsporadicallyBsimvastatinBsilverBshookBshakeyBsellingB
retrogradeBregB	prominentBprogressiveB	prescibedBpepcidB
oxybutyninBnappingB	miserablyBmethB	meanwhileBmanufacturingBlomediaBlayersB	keratosisB
interferesBingrownBhystericallyBhurryBhobbiesB
guidelinesBgrainBgoodbyeBgolytelyBestringBerrinB
eradicatedBenlargementB	emphysemaB
elementaryB
ejaculatedB
effectquotB	drawbacksB
dissolvingB
discussionBdiaryBdeathsBdangBcremeBcostingBcopaysBcookedB
containingBcompulsionsB
colchicineBchasingBbummedBbubbleBblotchesBarticlesBaggravatingB812B7dayB2hrsB2500B10sBweedBvoidBvioreleBvictimBupdatingBunnoticeableB
tizanidineBtablespoonsBsurroundingBsovaldiBsocksBsoccerBsleeveBsignedBshiftedBscreenB	reviewingBratingsB
ranitidineBracesBquotwhatB	preparingBphasesBpeaksBovertimeB
obsessionsB
normalizedBmovesBmindsetBmerelyBlonelyBklonipinBitu2019sB	irritatesBinstructionBindividualsB	incidentsB	identicalBicingBgreyBexperimentingBenablesBdryerBdistantBdipBdestructiveBdesertBdenavirB	defiantlyBdazeBdayiBdampcB	comparingBclothBcirculationBcircleBcigBceilingBbullBbikingBantacidsBamrixB
abnormallyB2530B208B176B167BxelodaBworsensBwinnerBwhereverBversesB
undergoingB
unbearablyB	strangersBsleBscoredBsauceBresistedB	resectionB
reconsiderBrangedBraisesBprofessionalsBprisonB	primidoneBpoliceBpluggedBperimenopauseBpastaBoutdoorsBnotingBnorabeB
nauseatingBmoderatesevereB
mastectomyBlitBlifelessBliedB	letrozoleBkgsBkeelBjournalBjoggingBintimacyB	influenceBinattentiveBimpulsivityB
imipramineBimageBhaywireB
glucophageB	fullblownBfosamaxBflexibilityBfillersBepipenBenzymeB	drugstoreBdraggingBdepthBdayquotBcytomelBcurbingB	countriesBcosB	complainsBcomparesBcombBcolaceB
clinicallyBclenchedBclaimingBchokeBcavedBcarafateBcageBcafergotBboozeBbootBbarsBavastinBadministeringBactivelyBaceB81B5000B31stB30mgsB270B1995B180mgBzocorBzepatierBweighsBwedgeBtruvadaBtraveledBtossedBteachingBtaughtBsuitableBshuttingBselfconsciousB
schedulingBscalyBsandalsBrodsBrestrictionBreassuranceBreapplyBratBquotoutBpricesBprecautionsBpourBpotsBoperateBnyquilBnowiBnorepinephrineBniaspanBnearestB	myrbetriqBmellowedBmaterialBlymphomaBlunaticBlotronexBlosesB
loloestrinBjerksBjealousB	interactsBindicateBincaseB	inbetweenBhugB
hoarsenessBhba1cBhayfeverBfortestaBfluctuatingBfishingBfindsBfaucetBfastestBexcruciatinglyBescapeBeliminationBelderlyBdozeBdosedBdistressingBdirtBdeservesBdeaBcoreB
commitmentBcleansB	bronchialBbeltB	awakeningBatopicBanaphylaxisBamphetaminesBamnesiaBaltBadjunctBacquiredB92B50mcgB1993BwelcomedBweirdestBviceBunsuccessfullyB	unfocusedBtrafficBtouchesBtodaysBthroatsBtextBstruckB
straightenBstintBstimulatingBstendraBspellingB	sincerelyBscamBretestedBrefrigeratedBpurelyBpregoB	petrifiedB	persevereBpartnersBophthalmologistB
motorcycleBmontelukastB
moderationBmenstruatingBmegaBmediaBmdsB	massivelyB
marginallyBlubeBlevlenBjellyBindiaBincapacitatedBimoBhpvBhandlingB
gallstonesBgainsBexcitingB
exacerbateB	educationBeagerBdividedBdiscouragingBcyclobenzaprineB	customersBcompromisedBcolcrysBcogentinBclinicsBcheatedBcannabisBcaBbuildupBbreakerB
boyfriendsBbookedB
basketballBattestBattackedBarousalBarguingBantacidBadvocateB77B5050B2lbsB280B265B184B128B10lbBwonBwelcholBweepingBwebsitesBvaniqaBvalacyclovirB	unplannedBtoolsBtiltedBsucceedBstabilizationBspiderBsnoringBsleepwalkingBskincareBskepticBsimultaneouslyB	screeningBrytaryBravenousBracedBquotyouBquotspottingquotB	processesBproceedBplateletBpigmentationB	overdriveB	obsessionBobservedBoaBnonsenseBnodeBnitsBnicodermBmutationBmonoBmetsBmequotBmaxedBlortabsBlooserBlocatedBlesionB	internistBidentifyBhpyloriBhorsesBhoarseB
hemoglobinBgradBgotoBflightsBflakeyBfiancBfacetB
everybodysBemergenciesB	elsewhereBeffectsquotB	dissipateBdemandBdecongestantBdeathlyB	concludedB	cognitionBcocaineB
clobetasolB	cirrhosisBcataractBbounceBboilBbeastBaxertBadhereBactavisBachinessB4hrsB30minsB235B200mgsB120lbsB117BzyclaraB	zopicloneByearsiBxeljanzBvitalBvastlyB	vasectomyBupwardsB	transientBtonedB	teenagersBsteppingBsotalolBsceneBscalingB
ropiniroleBrinsingBretainedB
repetitiveBrelativeBquotjustBpumpingBpruneB
processingB	plateletsB
paralyzingBpalBpackingBpackedB	pacemakerBoclockBobtainedBnonaddictiveBnetBndashBmeridiaBmeltedBmedrolBmarketedBmalaroneBlonglastingBliteralBlimpingBlidodermBlabiaBlabeledBkindergartenBititB	ingestingBindescribableB
incrementsB
impressionBheavesBhctzBgrainsBforeplayB
flashbacksBfileBfanaptBevaluateBdrawingBdraggedBdopeBdiphenhydramineBderealizationBdeliverBcrowdsBcowBclomipramineBclenchB
classifiedBcheersBbrilintaBbraveBbluesBblotchyBblastBbankBavidB	appealingBanimalBanesthesiologistBactemraBabusiveBabatedB97B730amB630amB5500B4000B32ozB245B20mgdayB198B160lbsB109BwedgesBthankyouBtailBtaclonexBsvtBsurroundingsBsurgeonsBstopedBspedBspBsoftballBslicedBsippingBsingingBshrankBsheetBseptraB
separatelyBseesBscrubBscentBroomsBrockingBrocketBrelistorBrejectedBrefundB
rationallyBramiprilBquitsBprohibitiveBpressingB	predictedBpolypBpeachBoldestB	occipitalBnearbyBnamendaBmidwifeB	metabolicBlivableBlingerBlensesBjerkBiraqB
hystericalBhypoglycemiaBhypnosisB	honeymoonBhcvBhalcionBgwBgrouchyBgarlicBgardenBfluttersBfeminineBfartherBfadesBexperimentedB
excitementBexceptionalBexaggerationB
enthusiasmBenglishBencouragingBelectrolyteBelatedBeffectsiB	dissolvedB	diabeticsBdevelopmentBdesoxynBdesiresB	dedicatedBdecentlyBdalirespBdabB
commentingBcliniqueBchiropracticB
chemicallyBcentsBcarsBcaracBblanketBbingingBbasketBautisticBanniversaryB58quotB2monthsB213B1aB1990B162B113BwrithingBwoundsBwarrantBvetBveraB	valsartanBureterBundergoBultrasoundsBtimedBtierBspriteBsportBservingB	sacrificeBreplyB	rejectingBrecievedBratioBpukedBpremenopausalBpostopBoveruseBoutburstBonestepBofflabelBneulastaBnauseousnessBnamedBmitralB	mediationBmanufacturedBmanufactureBlidB	lactuloseBlackedBlabourBl1BknotBjogBjcB
interviewsBinsignificantBinrB
indicationBincisionBiamBhumorBhostB
highschoolB
hesitationBhedBhavntBgodsBflutterBendepBemailBdyeBdonnatalBdistractBdilationBdeterioratingBdeployedBdatesBcrutchB
compressedBcmlBclimbedBchunksBbucketBbirdsBbingesBargumentB
admittedlyBactiqBacidicB94B930pmB5yrsB40mgsB180lbsB152B123B100mcgB02BzoladexBwifesBwelliBwalletBvicodenButterBtransitioningB
toothpasteBtearyBtbspBtarBsootheBroxyBringsBremotelyBregisterBreferralBrecordsBquotasB
questionedBprogressingB	pricelessBprevpacBpouringBplugBplanetBpasteBparkingB
nexaplanonBnewbornBmornBmommyBmexicoBmetroBmayoBmarvelonBmarrowBlowdoseB	lorazapamBjudgingB	isolationB	inheritedB	ingestionBindefinitelyB	impotenceBimmodiumBhypersensitiveBhospitalisedBhfaBheapsBharmingB	hangoversBgrindBgrassBglovesB
generationBfartB	extractedB	expressedBembrelBdopedBdilaudedBdevelopsBcrucialBconstructionBconsciousnessB	compliantBcaloricBcalmlyB	brisdelleBboringBbeeBbalmBawefulBaugmentBangeredB999B93B59quotB20yrsB1hrB150mgsB1230B120sB10500B1011B03BzumbaByuvafemByardsBwetnessBweakenedBvertebraBveltinB
vegetarianBtxBtshirtsB
truthfullyB	tradjentaBtonsillitisBthreadBsymbyaxB
suspiciousB	stumblingBstripBstitchesBspeedyB
somethingsBsheerBsepBseB
satisfyingBsaleBrythmolBroutinesB	routinelyBrogaineBrichBresponsibilitiesBradicalBquottakeBquotoffquotBquotmiracleBquotgoodquotBpunBprotonB	pressuredBppiBplantBpharmBperlesB	paperworkBpagesB	overdosedBoozingBoliveBoceanBnonnarcoticBnitroBnicerBnadaBmultiB	mouthwashBmarkedlyBlovehateBlimbBlightingBliesBlargestB
insurancesBhytrinBhyperhydrosisB
guaranteedB	gardeningBfourteenBfondBflowingBflewBflairBfinanciallyBfiftyBfancyBexclusivelyBerythromycinBdianeBd3B	correctedBcookiesBcontemplatedBconsequentlyBconfinedBclumsyBclosetBburstingBbraceBblurBblindingBblamingBbayerBbatteryB	backwardsB	attendingBaromasinBamergeB	akathesiaB25mgsB218B216B211B1994B1989B17lbsB175lbsB1600B15yrsBzonedBworldsBwinkBvisiblyBvesturaBtripledBtricksBtrailBtoxinsB	therequotBtendedBsystaneBswabBsteelBstareBsoothesBsingBsighBsiBshiversBsensoryBsendsBsatisfactoryBrizatriptanBrinsedB	repeatingBremediedB
registeredBrecollectionBreadilyBprednisoloneB	predisoneBplatesB
perseveredBoverthinkingBnostrilsBnormalcyBnonfunctionalBnegBnaB	multitudeBmountainBmensesB
meningitisBmallBlossesBloopBllysineBjackedBirregularlyBirregularityBinterestinglyB	installedBhypochondriaBhyperventilatingBholisticBhidB
herniationBguessedB
flutteringBfluctuationBfemalesB	fasciitisBfantasticallyBeyelidBexaggeratedBexacerbationBevidentB	epilepticB
enormouslyBenduringB
disruptiveBdetailedBdegeneratedB	deficientBdebilitatedBcrashesB
convincingB
congestiveBchaserB
buproprionBbrothersB
betterquotBbenzonatateBbathingBabsBabroadB630pmB54mgB430amB1gB1215B121B115lbsB06mgByouthB
yoursquoreBworkersBwindedB
widespreadBwhoppingBwellnessBvascularBtwistingBtwingeBtransdermalBtomatoBtenorminBtakinBsweaterBswappedBsurgeB
stutteringB	stressorsB	strenuousBstomachsBsprycelB	smokefreeBslideBsleevesB	shatteredB
separationB	sedentaryBsecureB	scenariosBrottenBridesB
restrictedBresolvesB
regularityBreclinerBreclastBratesBrangesBragesBquasenseBpregBpostersBperimenopausalBpaleoBorthostaticBorganB	operatingBoldsBnoraBnoddingB
neededquotBnasacortBmultaqBmoistBmobileBmirtazipineBmillionsBmicroBmenieresB	mechanismBligationB
lifesavingBleafletBkissBkapidexBjunkieBinflamationBhypochondriacBhiddenBhatefulB
grapefruitBgrandmaBgenuineBgeneticsBfurthermoreBfreezerBfibrosisBfalminaBexcusesBeverythingsBenglandBdullsBdrugscomBdistractingB	distancesB
disruptionBdilutedBdiarheaBdeptBdeedBcrushBcrotchBcountedBcmBclearestBchuggingBchildsBcheerfulBcheapestBcarduraBbustBbrinkBbreathedBblissBblandBbeginingB	balancingBawfullyBauthorizationB
attractiveBalopeciaBaclBaccompanyingBabtB	absorbingB6yrsB45lbsB320B30minB260B14090B127B100mgdayBwitchBweirdlyBvialB
unemployedBunconsciousBunavailableBtwistedBtwingesBtofranilBsympatheticBsumBstomachacheBspoonB	spiralingBsobBsicklyBshinBrehabsBrecreationalB
reappearedBraveB
quotweightBquotgetBquotcurequotBqualifyBpunctureBpsychologicallyBpseudoephedrineBproductivityB	pressuresBpossibilitiesB
performingBpelletsBpalsyBoxymorphoneBoutputB
osteopeniaBodtBoabB	nutrientsBnippleBmonthsiB
microgynonB
metabolizeBmasksB	managableBmalariaBlorzoneBloadedBlidsBleeryBlashBjustifyBjoinBjitterinessBjarB
hemorrhoidBheatedBheadachesmigrainesBhairlossBguidanceB
guanfacineBgreatfulBgoldenBfuseBfriesBflashingBflakeBfixingBfitbitBeyeballsB
esophagealBengageB	encounterB
ellipticalBellaoneBeegBechoBdwellBdressingBdiligentBcsBcommunicationBcolourBcavityB
carvedilolBcardsBbreakupBbloodsBblogsB	blisteredB	betaseronBberryBarrestBappendixBansweredBanorexicBannoyB	amplifiedB3yearsB233B194B189B158B14lbsB145lbsB1300B12pmB111B10kgB
zombielikeByoutubeByearoldBxanexBweeBwackyB	uroxatralBurgedBunresponsiveBunrelentingBunderactiveB
unbalancedBtricorBtrichBtrembleBtrainedBtissuesBtimequotBtidBthrowsBthirtiesB	thicknessBtemporalBtempleBswearsBstutterBstudentsB	stretchedB
stimulatorBspurBspiraledBsoftenedBslurringBsitsB	similarlyBsharperBshapedBshallowBservesBsensesBsenokotB	scratchedBscabsB
robitussinBriskingB
restartingBredderBravingBradioBquotinB	qualifiedBpursueBpromotedBprisonerBpottyBplagueBpillsdayBphysiologicalBpatentBpainiBpacingBoxideBoverloadBorapBocBobstructionBnothingsB	normalityBmonostatB
miscarriedBmethocarbamolBmeasuresB
manifestedB	localizedBleakedBlaparoscopyBkapvayB	judgementBjacketBitbutBinvolvesBinappropriateBiffyBhecticBgravesBgoodluckBfsBfractionBfluvoxamineB
fluctuatesBfirmlyBfidgetyBfaithfulBextractB	executiveBexaminationBevekeoBelephantBejaculationsBdonutBdisturbanceB
dependableB	decliningBcrpsBcraziestBcontractingBconsistsB
completionB	combatingBcimziaBchargedBchampBcentBbulkB	blackoutsBbitchBbiopsiesBbcbsBbakingBaveragedB	attractedBassistedBaprisoBanastrozoleBalasBafricanB	adulthoodBaddonB75325B3weeksB35mgB3060B226B202B188B182B174B16mgB168B125lbsB1216B116B114B1130pmB10mcgB100000B04B01mgB
yourselvesByB	wrenchingB	wonrsquotB	withdrawsBwettingBwaxBvoilaBvitBverballyBvascepaBunreasonableBtwinB	troublingB
triggeringB	tricyclicBtickB
substancesBstrainedBstinksB
stimulatedBsteriodBstainB	squeezingBsporanoxBsoulBshortactingBrudeB	remindingBregrowthB
regrettingBquotnotB	purposelyB
progressesBpreppingBprecipitatedBpokeBpmlBpinpointBpaymentBorthotriBobservationBobjectsBmultivitaminsBmosquitoBmoralBmmBmerckB	medicinalBmarinolBlitresB	ligamentsBlicenseBlemonsBlegitimatelyBjabBinterventionBintactBinjectorBingestBindocinB	indicatesB	impendingBhydrometBhsvB	healthilyBhardcoreBhaltBgreekBglowBgentlerBgatheredBgamblingBfunkBforemostBforearmBfloodingBfistulaB
fingertipsBfatsBexpelledBelectrolytesBefBdreamedBdownedB
documentedBdepletedB
deodorantsBdentBdemandedBdecidesBdaytodayBdampBdalmaneB	crazinessBcontributesBcontrastBconsultationBcomplicationB	combiventBcentreBbutiBburningitchingBbrickBbrasBbitchyBbandageBasksB
antifungalBanceB
amiodaroneBamdBagreeingBaggrenoxBafricaBabateB730pmB550B4yrsB48hrsB3daysB35lbsB24hrB2448B240mgB206B170lbsB157B135lbsB1213B105lbsB075BxmasBx3BwouldnrsquotBvideosBvelivetB
vancomycinBvagisilBurinatedBunmedicatedBunexplainableBticketBthirteenBsymptomaticBsupartzBsubstitutedB	stretchesBstarchBstainedBspinBsmoothedBsmhBshipB	seventeenBsensibleBsedateBscratchyBscenarioBsatisfyBsandBrippingB
resolutionB
reputationB	releasingBreleasesBquotimB
quantitiesBpullsBprunesBprofileB	primateneB
presentingBprefaceBpolishBpluckBpepperBpanelBpanBpakBpaintB
overpricedBosteoBorderingBoopsBoicBnutritionistBmydayisBmolarB	minimallyBmidstBmeatsBmcBmanagerBlybrelBloosenBlodgedBlingersB
lighteningBlifethreateningBlaminectomyBkefirBjokesBjesusBjawsBitchingburningB
infectiousBindexB	incapableB	impactingBibBhikeBhighwayBhemorrhagingBhcgBglueBgeniusBgenitalsBfuriousBfrontalBforcesBfmBflectorBflagBfissureB	fashionedB	fallopianB	escalatedBembeddedBedexB	dystrophyBdupixentBdoseageBdisagreeBdigestBdiahreaBdepressionsBdepressB	defectiveBcytotecBcrabbyB	copaymentB
compulsionB	collectedBcirclesBcandidaBbudBbricksBbgBatorvastatinBapplesB	anhedoniaBalarmedB
advantagesB510quotB350mgB25mcgB232B210lbsB187B173B166B132B124B122B11lbsB10mgdayB1030pmB010BzyvoxB
zonisamideBworthyBvicksBupwardBuntillBunknowinglyB
uneventfulBultimateB
turnaroundBtraceBtoughedBtoppedBtieBthesesB
technicianBtattoosBt3sBsuitedBstumbleB
stimulatesBstdsB
stabilizesBsoyBsoundingBsofaBserzoneBsepticBschoolsBsandozBsaltsB
reinsertedB	regrettedB
refreshingBrainBquothangoverquotB
queasinessBpulmonologistBprophylacticB	promotionBprografBprobsBpoxBphonedBperscriptionBperfumeBpepsiBpantBpanicsBonglyzaBofficerBnovartisBnotchBnotablyBnewlywedBndBmtB	minoxidilBmimicBmgdlBmethylprednisoloneBmeltdownBmastersB	marketingBmarkersBlotrelBlegitBleaksBlawsBkaiserBjBivsBiodineBinteractingBinfrequentlyBinfertilityBimpulsesB
imbalancesB	ideationsBicedB	hydratingBhumpBhsv2BhospitalizationsBhonorBholesBheedBgushingB	glyburideBfunkyBfriendshipsBfriBflBfishyBfilmsBfashionB	eyelashesBextendBexponentiallyBexpiresBesB	enduranceBeighteenBdramaBdoubtfulBdiveBdistinctBdissociationBdiggingBdeterB
describingBdefeatsBdefeatedBcyclicalBcyclicBconquerB
comfortingBcollectBcolaBcodeBcharlieBccB	cathetersBcarpelBcardiovascularBcankerBbrushedBbleachBbillionBbenlystaBbabysBb6BazuretteBappropriatelyBanimalsBaniextyBadolescenceBactinicB
accuratelyBabsurdBa1csB87B60mgsB209B1987B186B145mgB140lbsB12070B10daysB10dayB106ByupByisBwidelyBwateringBveBvaluesBuninterruptedBunevenBtwothreeBtrivialBtrilipixBtriglycerideBtighterBtightenBthroneBtemperaturesBtechBtattooBsurgerysBsummaryBspurtsBspiritsBspecializesBslicesBslaveBrobbedBrewardBresortedBremovesB	relationsB
reccommendBrangBquottooBquotcuredquotBquotbadquotBquotaB
quot10quotB	quarterlyB	qualitiesBpubicBpsychotherapyBpromoteB
professionBpretermB	practicalBpotatoesBpotatoB	postnasalBpillowsBpickyBphenobarbitalB
perforatedBpeakedBpassageBpapersB	overjoyedBoptimismBnonstimulantBmthfrBmircetteBmidwayBmentionsBmaskedBmarginalBlillyBlexBlearntBlazinessBlanguageBl3BjokingBiu2019veB	interestsB	intentionB
inevitableBimpulsivenessB
identifiedBhypoglycemicBhypersensitivityBhydroB
hormonallyBhandlesBhalvedBgrandchildrenBgirlfriendsBgaitBfmsBfellingBexploreBemptiedBemployerBelevateBejaculatingB
efficiencyBeducateB	downrightB	downfallsB
disciplineB
didnu2019tBdheBdetrimentalBdaydreamingBcrownB
creatinineBcontentsB
colleaguesBcollapseBcloudedBclearheadedB	classroomBchargingBcanasaBburpBbriskBboredomBblogBbleederBbeefBbeardBbarrierBbarleyBbangBavgBaversionBaugmentationBastBantiseizureBantiinflammatoriesBangleBamericaBalleviatingB
alleviatesBaideBagoraphobicBacknowledgeB
abstinenceB777B60mgdayB5mcgB360B300mgdayB2yearsB2dayB24hoursB246B222B219B200mgdayB1xdayB191B183B164B154ByelledBwrappingBwithingBweightedBwarmthBvoucherB	vacationsBupstairsB
unreliableBuniBunclearBtumourBtopicBtonicBtaxolBtanningBtanBsyringesBswitchesBsuppressiveBsupplementingBsumavelBstomacheBsqueezedBsprayedBspinsBspikingBsnowBsmarterBsippedBshutsB	separatedBseemingBschizophrenicBrespectivelyBrenderedB	religiousBrelationBrealizationBreadsBradiatesBquotitsBquotfeelquotBpyridiumBpsychotropicBpropelB
prometriumB	prevalentB
practicingBppdBpopsBpintBpinchesB	periactinBpeersB
paramedicsBpantoprazoleBovulesBoutdoorBofficialBoddsBoccurrencesB	normalizeB	neosporinBnakedBmyriadBmoisturizersBmodifiedBminipillBminesBmineralBmigranalBmidafternoonB
metastaticB
metastasisBmaniacBligamentBlifeiBlayedBlawsuitBjudgmentBi️BissuedBintroductionB	introduceB	interruptBinstantaneousBinsightBinducingB	imaginingBicyBickyBhyzaarBhypnoticBhyperthyroidismBhoseBheroBherbB
healthiestBhalvesB
hallelujahBhadhaveBgrowlingBgrilledBghostBgenoBgallonsBfreewayB	fragranceBformerlyBfolateBflukeBfamiliesBfacialsB	embolismsBeighthBeastBdriftBdietedB
detectableBdefectBcurvesBcurseBcreonBcosmeticBconvertB
compatibleBclipBchlorideBcarelessB
breastfeedBbreakthroughsBblowsBbloodstreamB	bloodshotBbismolBbextraB	benefitedB	balloonedBatkinsBarteriesBanoroBamarylB	alfuzosinBadzenysBadrenalBaampeB6mthsB625B2daysB24mgB248B230amB217B207B203B201B197B18lbsB165lbsB140sB110lbsBwhoveBwhamB	weirdnessB
weightgainB	washclothBwakefulnessBvytorinBvioxxBvialsBverifyB	vegetableBuvBuninterestedBunheardB	undergoneBunBtwoweekBtspBtrustingBtraumatizingBtonerBtitaniumBtickleBtibiaBthyroidectomyBthinsBthailandBtexasBtensBteasBswellsBswapBsustainBstemmingBspittingBsocketBsoapsBslumpBshavedB
schoolworkBroommateBrollsBriteB
reversibleBrequirementBrejectBrazorBquotpainB
quotmequotB	providersBprotectsB
protectingBpretendBpramipexoleBpostmenopausalBpostingsB
populationBplugsBpluckingBphysioBphendimetrazineBpeskyBperspirationB	pediatricBpartiesBovalBorthocyclenBopdivoBnorflexBnodularBnavyBnaprosynBmrBmintsBmigraneBmiBmeltsB	meltdownsBmelanomaBmeitBmarBmaoiBlocateBlistsBlimitationsBlightestBlegallyBleftoverBleaningBkeenBintravenouslyBingestedBhorizantBheadachyBhdBhazyBgraphicBgoofyBglimepirideBfuzzB	furnitureBfreaksB
forewarnedBflowsBflexBfissuresB
expirationBexchangeBexceedBevilsB	evidentlyBentocortB	emptinessB	elevationBeffortlesslyBdualBdoucheBdistractionsBdisfunctionBdiscomfortsBdicyclomineBdeteriorationB	desirableBdataBdarkestBcorticosteroidBcontrolquotB
complimentBclimateBcinnamonBchineseB
chatteringBcerebralB	celluliteB
cautiouslyB	cataractsBcarrierBcarisoprodolBcardizemB	carbatrolBcapletsBcampingBcampBcambiaBcaffieneBbudgetBbubblesBblinkBblanketsBbladeBbinBbearingBbathtubBaveenoB	attentiveBariceptBantiitchBanticonvulsantsBanticipatingBamoxclavB	amnesteemBamazonBaggressivelyBadderralB
activatingB
accidentalBabnormalitiesB5mlB511B40mgdayB3aB312B285B24feB230lbsB224B196B1820B181B179B169BwobblyBwinningBwhiplashBwdBwashroomBvitalsBvirginBverbalBventricularBvanishBvaluableBuptoB	unnaturalBuncleBucerisBturmericBtuneBtrippingB
tricyclicsBtransferredB
thrombosisBtenfoldBtbhBtbBtantrumsBsunshineBstunnedBstreetsBstemBstealingBspeedingBspasmingBspamsBsooooooBsneakBsluggishnessBsidedBsensitivitiesBsarafemBrxsBrotatingB	rituximabBrisenBrestorativeB	relativesBregimensBrefrigerateB
quotgoquotBquotdownBquickestBquestB	prostrateBprophylaxisBprojectBpricklyBprecancerousBpraisesBpoBplayerBpityBperiodspottingB
periodquotBpenetrationBpdB
pancreaticBpalpitationBpainquotBoutcomesBopticBobamaB
nexplannonBneurosurgeonBnaloxoneBmotilityBmothsBmonotherapyBmolyBmodernBmltBmiscarriagesBmgmtBmeltingBlevonorgestrelBlettersBlabelsBklonapinBkickerBkeysBkBiqBintravenousB	interviewBinsertsB
inoperableBincorporateBinclinedB	imiquimodBhygieneB	hydrationBhoorayBhoopsBgravolB	gratitudeBgraspBgpsBfreezeBfloraBfloodBfilterBextremlyBextensivelyB	expellingBexpelBexacerbationsB
evaluationBerasedBenthusiasticBelevatorBdysautonomiaBdrowningBdownerBdoorsBdistractionBdissatisfiedB	discountsBdifferB	diaphragmBdeliversBdaysiBcrutchesBcrustyBconceptB
compromiseBclubBclickBcitrusBchatBcatsBcatapresBcardioversionBcanceledBcamilaBc7BbutterfliesB
budesonideBbrilliantlyB
breathlessBbouncedBbottledBbotchedBbiweeklyBbicycleB	bathroomsB	bariatricB
azelastineBazBapetiteBantsyBantiperspirantB
anorgasmiaBambitionBairportBairplaneBaimBaccountableBabcessB6monthB615B612B520B365B2mgsB214B1monthB1985B163B155lbsB	zombifiedBzipBwtfBvoidingBvireadBviibyrdB
vasculitisBurgingBunsafeBtzoneBtwistBtrichotillomaniaBtrendBtiaBthreadsBthisquotB
therapistsBterminalBteenyBtbiBtasignaB	tadalafilBsupplementedBsupplementalBsubsysB	structureB	strangestBstabBsquirtsBsportingB
spasticityBsoothedBsolvesBsoleBsmoothieBsilenceBsignalsBshuntBshieldBseasonsB	scaledownBsaviourBsanitaryBrubberBrotateBrodeBreliantBrelafenBrefrainB	refillingBreducerBreddishBrecluseB
reassuringBreappearBragBquotzapsquotBquestionableBpropafenoneBpreparationsBpoweradeBpokedBplungeB	pillsquotBpilatesBpiercingB	perpetualB
permissionB	penetrateB
peacefullyBpbsBparBpanickB	orthoceptBoperatedBopensBomnicefBodourBnosesBnortrelBnonexistantB	mortifiedBmoaningBmethylinBmestinonBmendBmaoisBlogicalBlistlessBlipidBliftsBlifequotB
liberatingBlevsinBlaughterBlatexB	lastacaftB
laryngitisBintroducingBinsanityBinhaleBincorrectlyBincapacitatingBimpactsBhypertensiveBhumiliatingBhsBhorrorsBhonorsBhiprexBhandfulsBgoodquotBgatesBgamutBfycompaB
furosemideBfocalBfloorsB	fidgetingBfibBfentoraBfeelingquotBfecalB	favorableBfathersBeyebrowB	excedrineBestablishedBequateBepilimBenvironmentalBemendBemailedBemBearnedBdwellingBdutiesBdownquotBdoubtedB	distendedBdippedBdigBdexBdeterminationB	describesBdawnBdangersB	cypionateBcozB	cortizoneBcoronaryB	convertedBconsecutivelyBcomorbidBcommunicatingB
colonscopyBcleansedBclampedBchurningBchunkyBchokedBchillingBchalkyB
cetirizineBcerebriBcasodexBcartiaBcarefreeBc5BbreathlessnessB	blockagesBblinkingB	blessingsBbladesB	bisacodylBbiotinBbetablockerBbbBbaggyB
astoundingB
articulateB
applesauceBanydayBanticipationB	answeringBampedB	allergensBalittleBairwaysBagoiB	acceptingB	abscessedBabruptB530amB300sB2wksB255B250lbsB22lbsB192B159B156B12amB10ozB1000xB	worryfreeBwithdrewB	whiteheadBweeksiBwbcBwbBwatersBwaneBwafersBveramystBvagueBuprightBunfairBundiesBuhBtudorzaBtsBtryedBtruelyBtransitionedBtiringBtireBticksB
testicularB	testiclesBtapersBtalwinBswBsueB	succinateBstrungBstrangerBstarveBsprainedBspondylosisB
somnolenceBsomedaysB	smoothiesBsmiledBslewB
sildenafilBshittyBsepsisBseedB
scientistsBscabbedBsaBrtBresearchersBreproductiveBrenewalBremarksBrebateB	reasoningBrapBradiatedBquotwellBquotmiraclequotB	prostaticB	professorB
prescriberBpremproBpremenopauseBpraluentBpouchB	posteriorBpoofBplungerBpilingB	phosphateB	paramedicBpairsBorganizationBofferingBnutB
normalquotBnizoralBnitrateBnightmarishBneurotinB
negligibleBnecrosisBnationalBmphBmorbidlyBmonBmissionBmereBmenstralBmapBmanagesB	lunchtimeBloverBlovenoxBlorcetBlongtimeBlingeredBkudosBjohnsonBitemBischemicBirritationsBirresponsibleBirrationallyBironicBintervalB
insertionsBindoorB
imperativeBhypedBhydrochlorideB	hostilityBhockeyBhiatalBherionBhardnessBguttateB	groceriesB	grandkidsBgrandfatherB
graduationBgiddyBgambleBgalBgainlossBfloB	flavoringBflankBfibreBfemconBfailuresBeyleaB	explosionBexperimentalB	exfoliateBexaminedBerrorsBentiretyBencouragementBenablingBdysmenorrheaBdysfunctionalBdoomedBdisorientatedBdismayB
discoloredBdilatingBdialysisBdenyBdemonB	definetlyBdeficitBdawnedBdancerBcursedBculturesBcracksBcourtBconvulsionsBconservativeBcompanysB	colleagueBcoherentBcoalB	clozapineB	clonodineBchalkB	certifiedB	celebrateBcaveatBburgerBbluntedBblackoutBbiasedBbdB	autonomicB	attackingBarthroscopicBarrivalBarrayBappendicitisB
anxiolyticBanteriorBangeliqBanaestheticB	amazementBagainiB	accompanyB
acclimatedB830pmB830amB75mgdayB70lbsB6xB5mmB5mgdayB50000B4monthsB300mgsB2gB253B244B20lbB2018B1yrB16lbsB142B138B1030amBzegeridByoyoBworkdayBwiselyBwheezeBwateredBwasisBwanedBviolenceBvegBumBtwitchyBtweakBtrunkBtrulanceBtricareBtriamcinoloneBtraumatizedBtransferBtoddlersB	timeframeBthatllBtartrateB	survivingBsunkBstrickenBstrainsBstoleBspilledBsourcesBskinnedBsiboBshoeBsettingsBseraxBselectedB	sedativesBsealedBscrapeBscoresBscissorsB
sacroiliacBrumblesB
retirementBrestaurantsBrespimatB
respiclickBrecheckBreassureBquotthisBquotregularquotB	quotbreakBquotbetterquotBqtipBprolongBpowersBpoweredBpowderedBpopsicleBpileBphobiasB	persuadedBpercodanBpendingBoxytrolBoxcarbazepineBoverpoweringBoverdoBorganizeBondansetronBodBobliteratedBnyBnotifiedBnilBnarcolepticBnappedBmustacheBmoldB
moisturiseBmodestBmessagesBmeasurementBmalsBltB	lopressorBlevoBlevetiracetamBlectureBlandingB	lactatingBlaboredB	isentressBiquotmBinvestigationBintoxicatedB	infertileB
incompleteBhypnotherapyBhpB
houseboundBhotcoldBhopBhomelessB
heartbeatsBharmlessBhabitualBgunkBgrease
??
Const_5Const*
_output_shapes	
:?N*
dtype0	*??
value??B??	?N"??                                                 	       
                                                                                                                                                                  !       "       #       $       %       &       '       (       )       *       +       ,       -       .       /       0       1       2       3       4       5       6       7       8       9       :       ;       <       =       >       ?       @       A       B       C       D       E       F       G       H       I       J       K       L       M       N       O       P       Q       R       S       T       U       V       W       X       Y       Z       [       \       ]       ^       _       `       a       b       c       d       e       f       g       h       i       j       k       l       m       n       o       p       q       r       s       t       u       v       w       x       y       z       {       |       }       ~              ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?                                                              	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?       	      	      	      	      	      	      	      	      	      		      
	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	       	      !	      "	      #	      $	      %	      &	      '	      (	      )	      *	      +	      ,	      -	      .	      /	      0	      1	      2	      3	      4	      5	      6	      7	      8	      9	      :	      ;	      <	      =	      >	      ?	      @	      A	      B	      C	      D	      E	      F	      G	      H	      I	      J	      K	      L	      M	      N	      O	      P	      Q	      R	      S	      T	      U	      V	      W	      X	      Y	      Z	      [	      \	      ]	      ^	      _	      `	      a	      b	      c	      d	      e	      f	      g	      h	      i	      j	      k	      l	      m	      n	      o	      p	      q	      r	      s	      t	      u	      v	      w	      x	      y	      z	      {	      |	      }	      ~	      	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	       
      
      
      
      
      
      
      
      
      	
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       
      !
      "
      #
      $
      %
      &
      '
      (
      )
      *
      +
      ,
      -
      .
      /
      0
      1
      2
      3
      4
      5
      6
      7
      8
      9
      :
      ;
      <
      =
      >
      ?
      @
      A
      B
      C
      D
      E
      F
      G
      H
      I
      J
      K
      L
      M
      N
      O
      P
      Q
      R
      S
      T
      U
      V
      W
      X
      Y
      Z
      [
      \
      ]
      ^
      _
      `
      a
      b
      c
      d
      e
      f
      g
      h
      i
      j
      k
      l
      m
      n
      o
      p
      q
      r
      s
      t
      u
      v
      w
      x
      y
      z
      {
      |
      }
      ~
      
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                                      	       
                                                                                                                                                                  !       "       #       $       %       &       '       (       )       *       +       ,       -       .       /       0       1       2       3       4       5       6       7       8       9       :       ;       <       =       >       ?       @       A       B       C       D       E       F       G       H       I       J       K       L       M       N       O       P       Q       R       S       T       U       V       W       X       Y       Z       [       \       ]       ^       _       `       a       b       c       d       e       f       g       h       i       j       k       l       m       n       o       p       q       r       s       t       u       v       w       x       y       z       {       |       }       ~              ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?        !      !      !      !      !      !      !      !      !      	!      
!      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !      !       !      !!      "!      #!      $!      %!      &!      '!      (!      )!      *!      +!      ,!      -!      .!      /!      0!      1!      2!      3!      4!      5!      6!      7!      8!      9!      :!      ;!      <!      =!      >!      ?!      @!      A!      B!      C!      D!      E!      F!      G!      H!      I!      J!      K!      L!      M!      N!      O!      P!      Q!      R!      S!      T!      U!      V!      W!      X!      Y!      Z!      [!      \!      ]!      ^!      _!      `!      a!      b!      c!      d!      e!      f!      g!      h!      i!      j!      k!      l!      m!      n!      o!      p!      q!      r!      s!      t!      u!      v!      w!      x!      y!      z!      {!      |!      }!      ~!      !      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!      ?!       "      "      "      "      "      "      "      "      "      	"      
"      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "      "       "      !"      ""      #"      $"      %"      &"      '"      ("      )"      *"      +"      ,"      -"      ."      /"      0"      1"      2"      3"      4"      5"      6"      7"      8"      9"      :"      ;"      <"      ="      >"      ?"      @"      A"      B"      C"      D"      E"      F"      G"      H"      I"      J"      K"      L"      M"      N"      O"      P"      Q"      R"      S"      T"      U"      V"      W"      X"      Y"      Z"      ["      \"      ]"      ^"      _"      `"      a"      b"      c"      d"      e"      f"      g"      h"      i"      j"      k"      l"      m"      n"      o"      p"      q"      r"      s"      t"      u"      v"      w"      x"      y"      z"      {"      |"      }"      ~"      "      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"      ?"       #      #      #      #      #      #      #      #      #      	#      
#      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #      #       #      !#      "#      ##      $#      %#      &#      '#      (#      )#      *#      +#      ,#      -#      .#      /#      0#      1#      2#      3#      4#      5#      6#      7#      8#      9#      :#      ;#      <#      =#      >#      ?#      @#      A#      B#      C#      D#      E#      F#      G#      H#      I#      J#      K#      L#      M#      N#      O#      P#      Q#      R#      S#      T#      U#      V#      W#      X#      Y#      Z#      [#      \#      ]#      ^#      _#      `#      a#      b#      c#      d#      e#      f#      g#      h#      i#      j#      k#      l#      m#      n#      o#      p#      q#      r#      s#      t#      u#      v#      w#      x#      y#      z#      {#      |#      }#      ~#      #      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#      ?#       $      $      $      $      $      $      $      $      $      	$      
$      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $      $       $      !$      "$      #$      $$      %$      &$      '$      ($      )$      *$      +$      ,$      -$      .$      /$      0$      1$      2$      3$      4$      5$      6$      7$      8$      9$      :$      ;$      <$      =$      >$      ?$      @$      A$      B$      C$      D$      E$      F$      G$      H$      I$      J$      K$      L$      M$      N$      O$      P$      Q$      R$      S$      T$      U$      V$      W$      X$      Y$      Z$      [$      \$      ]$      ^$      _$      `$      a$      b$      c$      d$      e$      f$      g$      h$      i$      j$      k$      l$      m$      n$      o$      p$      q$      r$      s$      t$      u$      v$      w$      x$      y$      z$      {$      |$      }$      ~$      $      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$      ?$       %      %      %      %      %      %      %      %      %      	%      
%      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %      %       %      !%      "%      #%      $%      %%      &%      '%      (%      )%      *%      +%      ,%      -%      .%      /%      0%      1%      2%      3%      4%      5%      6%      7%      8%      9%      :%      ;%      <%      =%      >%      ?%      @%      A%      B%      C%      D%      E%      F%      G%      H%      I%      J%      K%      L%      M%      N%      O%      P%      Q%      R%      S%      T%      U%      V%      W%      X%      Y%      Z%      [%      \%      ]%      ^%      _%      `%      a%      b%      c%      d%      e%      f%      g%      h%      i%      j%      k%      l%      m%      n%      o%      p%      q%      r%      s%      t%      u%      v%      w%      x%      y%      z%      {%      |%      }%      ~%      %      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%      ?%       &      &      &      &      &      &      &      &      &      	&      
&      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &      &       &      !&      "&      #&      $&      %&      &&      '&      (&      )&      *&      +&      ,&      -&      .&      /&      0&      1&      2&      3&      4&      5&      6&      7&      8&      9&      :&      ;&      <&      =&      >&      ?&      @&      A&      B&      C&      D&      E&      F&      G&      H&      I&      J&      K&      L&      M&      N&      O&      P&      Q&      R&      S&      T&      U&      V&      W&      X&      Y&      Z&      [&      \&      ]&      ^&      _&      `&      a&      b&      c&      d&      e&      f&      g&      h&      i&      j&      k&      l&      m&      n&      o&      p&      q&      r&      s&      t&      u&      v&      w&      x&      y&      z&      {&      |&      }&      ~&      &      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&      ?&       '      '      '      '      '      '      '      '      '      	'      
'      '      '      '      '      '      
?
StatefulPartitionedCallStatefulPartitionedCall
hash_tableConst_4Const_5*
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *$
fR
__inference_<lambda>_240981
?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *$
fR
__inference_<lambda>_240986
8
NoOpNoOp^PartitionedCall^StatefulPartitionedCall
?
?MutableHashTable_lookup_table_export_values/LookupTableExportV2LookupTableExportV2MutableHashTable*
Tkeys0*
Tvalues0	*#
_class
loc:@MutableHashTable*
_output_shapes

::
?)
Const_6Const"/device:CPU:0*
_output_shapes
: *
dtype0*?(
value?(B?( B?(
?
layer-0
layer_with_weights-0
layer-1
layer_with_weights-1
layer-2
layer-3
layer_with_weights-2
layer-4
layer-5
layer_with_weights-3
layer-6
	optimizer
		variables

trainable_variables
regularization_losses
	keras_api

signatures
 
"
_lookup_layer
	keras_api
b

embeddings
	variables
trainable_variables
regularization_losses
	keras_api
R
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
R
	variables
 trainable_variables
!regularization_losses
"	keras_api
h

#kernel
$bias
%	variables
&trainable_variables
'regularization_losses
(	keras_api
?
)iter

*beta_1

+beta_2
	,decay
-learning_ratem[m\m]#m^$m_v`vavb#vc$vd
#
1
2
3
#4
$5
#
0
1
2
#3
$4
 
?
.non_trainable_variables

/layers
0metrics
1layer_regularization_losses
2layer_metrics
		variables

trainable_variables
regularization_losses
 
3
3lookup_table
4token_counts
5	keras_api
 
fd
VARIABLE_VALUEembedding_6/embeddings:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUE

0

0
 
?
6non_trainable_variables

7layers
8metrics
9layer_regularization_losses
:layer_metrics
	variables
trainable_variables
regularization_losses
 
 
 
?
;non_trainable_variables

<layers
=metrics
>layer_regularization_losses
?layer_metrics
	variables
trainable_variables
regularization_losses
[Y
VARIABLE_VALUEdense_13/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE
WU
VARIABLE_VALUEdense_13/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
?
@non_trainable_variables

Alayers
Bmetrics
Clayer_regularization_losses
Dlayer_metrics
	variables
trainable_variables
regularization_losses
 
 
 
?
Enon_trainable_variables

Flayers
Gmetrics
Hlayer_regularization_losses
Ilayer_metrics
	variables
 trainable_variables
!regularization_losses
[Y
VARIABLE_VALUEdense_12/kernel6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUE
WU
VARIABLE_VALUEdense_12/bias4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUE

#0
$1

#0
$1
 
?
Jnon_trainable_variables

Klayers
Lmetrics
Mlayer_regularization_losses
Nlayer_metrics
%	variables
&trainable_variables
'regularization_losses
HF
VARIABLE_VALUE	Adam/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
LJ
VARIABLE_VALUEAdam/beta_1+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUE
LJ
VARIABLE_VALUEAdam/beta_2+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUE
JH
VARIABLE_VALUE
Adam/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
ZX
VARIABLE_VALUEAdam/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE
 
1
0
1
2
3
4
5
6

O0
P1
 
 

Q_initializer
LJ
tableAlayer_with_weights-0/_lookup_layer/token_counts/.ATTRIBUTES/table
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
4
	Rtotal
	Scount
T	variables
U	keras_api
D
	Vtotal
	Wcount
X
_fn_kwargs
Y	variables
Z	keras_api
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

R0
S1

T	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE
 

V0
W1

Y	variables
??
VARIABLE_VALUEAdam/embedding_6/embeddings/mVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUEAdam/dense_13/kernel/mRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
zx
VARIABLE_VALUEAdam/dense_13/bias/mPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUEAdam/dense_12/kernel/mRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
zx
VARIABLE_VALUEAdam/dense_12/bias/mPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUEAdam/embedding_6/embeddings/vVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUEAdam/dense_13/kernel/vRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
zx
VARIABLE_VALUEAdam/dense_13/bias/vPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUEAdam/dense_12/kernel/vRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
zx
VARIABLE_VALUEAdam/dense_12/bias/vPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
z
serving_default_input_7Placeholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
?
StatefulPartitionedCall_1StatefulPartitionedCallserving_default_input_7
hash_tableConstConst_1Const_2embedding_6/embeddingsdense_13/kerneldense_13/biasdense_12/kerneldense_12/bias*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*0
config_proto 

CPU

GPU2*0J 8? *-
f(R&
$__inference_signature_wrapper_240552
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?

StatefulPartitionedCall_2StatefulPartitionedCallsaver_filename*embedding_6/embeddings/Read/ReadVariableOp#dense_13/kernel/Read/ReadVariableOp!dense_13/bias/Read/ReadVariableOp#dense_12/kernel/Read/ReadVariableOp!dense_12/bias/Read/ReadVariableOpAdam/iter/Read/ReadVariableOpAdam/beta_1/Read/ReadVariableOpAdam/beta_2/Read/ReadVariableOpAdam/decay/Read/ReadVariableOp&Adam/learning_rate/Read/ReadVariableOp?MutableHashTable_lookup_table_export_values/LookupTableExportV2AMutableHashTable_lookup_table_export_values/LookupTableExportV2:1total/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOp1Adam/embedding_6/embeddings/m/Read/ReadVariableOp*Adam/dense_13/kernel/m/Read/ReadVariableOp(Adam/dense_13/bias/m/Read/ReadVariableOp*Adam/dense_12/kernel/m/Read/ReadVariableOp(Adam/dense_12/bias/m/Read/ReadVariableOp1Adam/embedding_6/embeddings/v/Read/ReadVariableOp*Adam/dense_13/kernel/v/Read/ReadVariableOp(Adam/dense_13/bias/v/Read/ReadVariableOp*Adam/dense_12/kernel/v/Read/ReadVariableOp(Adam/dense_12/bias/v/Read/ReadVariableOpConst_6*'
Tin 
2		*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *(
f#R!
__inference__traced_save_241095
?
StatefulPartitionedCall_3StatefulPartitionedCallsaver_filenameembedding_6/embeddingsdense_13/kerneldense_13/biasdense_12/kerneldense_12/bias	Adam/iterAdam/beta_1Adam/beta_2
Adam/decayAdam/learning_rateMutableHashTabletotalcounttotal_1count_1Adam/embedding_6/embeddings/mAdam/dense_13/kernel/mAdam/dense_13/bias/mAdam/dense_12/kernel/mAdam/dense_12/bias/mAdam/embedding_6/embeddings/vAdam/dense_13/kernel/vAdam/dense_13/bias/vAdam/dense_12/kernel/vAdam/dense_12/bias/v*%
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *+
f&R$
"__inference__traced_restore_241180??

?
-
__inference__destroyer_240939
identityG
ConstConst*
_output_shapes
: *
dtype0*
value	B :E
IdentityIdentityConst:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?C
?
__inference_adapt_step_240801
iterator

iterator_19
5none_lookup_table_find_lookuptablefindv2_table_handle:
6none_lookup_table_find_lookuptablefindv2_default_value	??IteratorGetNext?(None_lookup_table_find/LookupTableFindV2?,None_lookup_table_insert/LookupTableInsertV2?
IteratorGetNextIteratorGetNextiterator*
_class
loc:@iterator*#
_output_shapes
:?????????*"
output_shapes
:?????????*
output_types
2]
StringLowerStringLowerIteratorGetNext:components:0*#
_output_shapes
:??????????
StaticRegexReplaceStaticRegexReplaceStringLower:output:0*#
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite R
StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
StringSplit/StringSplitV2StringSplitV2StaticRegexReplace:output:0StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:p
StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        r
!StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       r
!StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
StringSplit/strided_sliceStridedSlice#StringSplit/StringSplitV2:indices:0(StringSplit/strided_slice/stack:output:0*StringSplit/strided_slice/stack_1:output:0*StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_maskk
!StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: m
#StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:m
#StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
StringSplit/strided_slice_1StridedSlice!StringSplit/StringSplitV2:shape:0*StringSplit/strided_slice_1/stack:output:0,StringSplit/strided_slice_1/stack_1:output:0,StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
BStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast"StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast$StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShapeFStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
KStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdUStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0UStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
PStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreaterTStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0YStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
KStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastRStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMaxFStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0WStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2SStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0UStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMulOStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximumHStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimumHStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
OStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincountFStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0WStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
IStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumVStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
MStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
IStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2VStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
UniqueWithCountsUniqueWithCounts"StringSplit/StringSplitV2:values:0*
T0*A
_output_shapes/
-:?????????:?????????:?????????*
out_idx0	?
(None_lookup_table_find/LookupTableFindV2LookupTableFindV25none_lookup_table_find_lookuptablefindv2_table_handleUniqueWithCounts:y:06none_lookup_table_find_lookuptablefindv2_default_value",/job:localhost/replica:0/task:0/device:CPU:0*	
Tin0*

Tout0	*
_output_shapes
:|
addAddV2UniqueWithCounts:count:01None_lookup_table_find/LookupTableFindV2:values:0*
T0	*
_output_shapes
:?
,None_lookup_table_insert/LookupTableInsertV2LookupTableInsertV25none_lookup_table_find_lookuptablefindv2_table_handleUniqueWithCounts:y:0add:z:0)^None_lookup_table_find/LookupTableFindV2",/job:localhost/replica:0/task:0/device:CPU:0*	
Tin0*

Tout0	*
_output_shapes
 *(
_construction_contextkEagerRuntime*
_input_shapes

: : : : 2"
IteratorGetNextIteratorGetNext2T
(None_lookup_table_find/LookupTableFindV2(None_lookup_table_find/LookupTableFindV22\
,None_lookup_table_insert/LookupTableInsertV2,None_lookup_table_insert/LookupTableInsertV2:( $
"
_user_specified_name
iterator:@<

_output_shapes
: 
"
_user_specified_name
iterator:

_output_shapes
: 
?

?
D__inference_dense_13_layer_call_and_return_conditional_losses_240143

inputs1
matmul_readvariableop_resource:	?-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????P
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????a
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
n
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240130

inputs
identityW
Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :e
MaxMaxinputsMax/reduction_indices:output:0*
T0*(
_output_shapes
:??????????U
IdentityIdentityMax:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:?????????d?:T P
,
_output_shapes
:?????????d?
 
_user_specified_nameinputs
?m
?
C__inference_model_6_layer_call_and_return_conditional_losses_240174

inputsT
Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handleU
Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	1
-text_vectorization_6_string_lookup_12_equal_y4
0text_vectorization_6_string_lookup_12_selectv2_t	&
embedding_6_240122:
?N?"
dense_13_240144:	?
dense_13_240146:!
dense_12_240168:
dense_12_240170:
identity?? dense_12/StatefulPartitionedCall? dense_13/StatefulPartitionedCall?#embedding_6/StatefulPartitionedCall?Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2`
 text_vectorization_6/StringLowerStringLowerinputs*'
_output_shapes
:??????????
'text_vectorization_6/StaticRegexReplaceStaticRegexReplace)text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
text_vectorization_6/SqueezeSqueeze0text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????g
&text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
.text_vectorization_6/StringSplit/StringSplitV2StringSplitV2%text_vectorization_6/Squeeze:output:0/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
4text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
6text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
6text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
.text_vectorization_6/StringSplit/strided_sliceStridedSlice8text_vectorization_6/StringSplit/StringSplitV2:indices:0=text_vectorization_6/StringSplit/strided_slice/stack:output:0?text_vectorization_6/StringSplit/strided_slice/stack_1:output:0?text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
6text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
8text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
8text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
0text_vectorization_6/StringSplit/strided_slice_1StridedSlice6text_vectorization_6/StringSplit/StringSplitV2:shape:0?text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
Wtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
etext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
dtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
btext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle7text_vectorization_6/StringSplit/StringSplitV2:values:0Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
+text_vectorization_6/string_lookup_12/EqualEqual7text_vectorization_6/StringSplit/StringSplitV2:values:0-text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/SelectV2SelectV2/text_vectorization_6/string_lookup_12/Equal:z:00text_vectorization_6_string_lookup_12_selectv2_tLtext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/IdentityIdentity7text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????s
1text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
)text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
8text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor2text_vectorization_6/RaggedToTensor/Const:output:07text_vectorization_6/string_lookup_12/Identity:output:0:text_vectorization_6/RaggedToTensor/default_value:output:09text_vectorization_6/StringSplit/strided_slice_1:output:07text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
#embedding_6/StatefulPartitionedCallStatefulPartitionedCallAtext_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0embedding_6_240122*
Tin
2	*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:?????????d?*#
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *P
fKRI
G__inference_embedding_6_layer_call_and_return_conditional_losses_240121?
&global_max_pooling1d_6/PartitionedCallPartitionedCall,embedding_6/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *[
fVRT
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240130?
 dense_13/StatefulPartitionedCallStatefulPartitionedCall/global_max_pooling1d_6/PartitionedCall:output:0dense_13_240144dense_13_240146*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_13_layer_call_and_return_conditional_losses_240143?
dropout_6/PartitionedCallPartitionedCall)dense_13/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *N
fIRG
E__inference_dropout_6_layer_call_and_return_conditional_losses_240154?
 dense_12/StatefulPartitionedCallStatefulPartitionedCall"dropout_6/PartitionedCall:output:0dense_12_240168dense_12_240170*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_12_layer_call_and_return_conditional_losses_240167x
IdentityIdentity)dense_12/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp!^dense_12/StatefulPartitionedCall!^dense_13/StatefulPartitionedCall$^embedding_6/StatefulPartitionedCallD^text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_12/StatefulPartitionedCall dense_12/StatefulPartitionedCall2D
 dense_13/StatefulPartitionedCall dense_13/StatefulPartitionedCall2J
#embedding_6/StatefulPartitionedCall#embedding_6/StatefulPartitionedCall2?
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?

?
(__inference_model_6_layer_call_fn_240387
input_7
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:
?N?
	unknown_4:	?
	unknown_5:
	unknown_6:
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinput_7unknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*0
config_proto 

CPU

GPU2*0J 8? *L
fGRE
C__inference_model_6_layer_call_and_return_conditional_losses_240343o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_7:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?g
?
"__inference__traced_restore_241180
file_prefix;
'assignvariableop_embedding_6_embeddings:
?N?5
"assignvariableop_1_dense_13_kernel:	?.
 assignvariableop_2_dense_13_bias:4
"assignvariableop_3_dense_12_kernel:.
 assignvariableop_4_dense_12_bias:&
assignvariableop_5_adam_iter:	 (
assignvariableop_6_adam_beta_1: (
assignvariableop_7_adam_beta_2: '
assignvariableop_8_adam_decay: /
%assignvariableop_9_adam_learning_rate: M
Cmutablehashtable_table_restore_lookuptableimportv2_mutablehashtable: #
assignvariableop_10_total: #
assignvariableop_11_count: %
assignvariableop_12_total_1: %
assignvariableop_13_count_1: E
1assignvariableop_14_adam_embedding_6_embeddings_m:
?N?=
*assignvariableop_15_adam_dense_13_kernel_m:	?6
(assignvariableop_16_adam_dense_13_bias_m:<
*assignvariableop_17_adam_dense_12_kernel_m:6
(assignvariableop_18_adam_dense_12_bias_m:E
1assignvariableop_19_adam_embedding_6_embeddings_v:
?N?=
*assignvariableop_20_adam_dense_13_kernel_v:	?6
(assignvariableop_21_adam_dense_13_bias_v:<
*assignvariableop_22_adam_dense_12_kernel_v:6
(assignvariableop_23_adam_dense_12_bias_v:
identity_25??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_16?AssignVariableOp_17?AssignVariableOp_18?AssignVariableOp_19?AssignVariableOp_2?AssignVariableOp_20?AssignVariableOp_21?AssignVariableOp_22?AssignVariableOp_23?AssignVariableOp_3?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?AssignVariableOp_9?2MutableHashTable_table_restore/LookupTableImportV2?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value?B?B:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEBFlayer_with_weights-0/_lookup_layer/token_counts/.ATTRIBUTES/table-keysBHlayer_with_weights-0/_lookup_layer/token_counts/.ATTRIBUTES/table-valuesB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*I
value@B>B B B B B B B B B B B B B B B B B B B B B B B B B B B ?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*?
_output_shapesn
l:::::::::::::::::::::::::::*)
dtypes
2		[
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOpAssignVariableOp'assignvariableop_embedding_6_embeddingsIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_1AssignVariableOp"assignvariableop_1_dense_13_kernelIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_2AssignVariableOp assignvariableop_2_dense_13_biasIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_3AssignVariableOp"assignvariableop_3_dense_12_kernelIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_4AssignVariableOp assignvariableop_4_dense_12_biasIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0	*
_output_shapes
:?
AssignVariableOp_5AssignVariableOpassignvariableop_5_adam_iterIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	]

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_6AssignVariableOpassignvariableop_6_adam_beta_1Identity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_7AssignVariableOpassignvariableop_7_adam_beta_2Identity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_8AssignVariableOpassignvariableop_8_adam_decayIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_9AssignVariableOp%assignvariableop_9_adam_learning_rateIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype0?
2MutableHashTable_table_restore/LookupTableImportV2LookupTableImportV2Cmutablehashtable_table_restore_lookuptableimportv2_mutablehashtableRestoreV2:tensors:10RestoreV2:tensors:11*	
Tin0*

Tout0	*#
_class
loc:@MutableHashTable*
_output_shapes
 _
Identity_10IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_10AssignVariableOpassignvariableop_10_totalIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_11IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_11AssignVariableOpassignvariableop_11_countIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_12IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_12AssignVariableOpassignvariableop_12_total_1Identity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_13IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_13AssignVariableOpassignvariableop_13_count_1Identity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_14IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_14AssignVariableOp1assignvariableop_14_adam_embedding_6_embeddings_mIdentity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_15IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_15AssignVariableOp*assignvariableop_15_adam_dense_13_kernel_mIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_16IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_16AssignVariableOp(assignvariableop_16_adam_dense_13_bias_mIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_17IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_17AssignVariableOp*assignvariableop_17_adam_dense_12_kernel_mIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_18IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_18AssignVariableOp(assignvariableop_18_adam_dense_12_bias_mIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_19IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_19AssignVariableOp1assignvariableop_19_adam_embedding_6_embeddings_vIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_20IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_20AssignVariableOp*assignvariableop_20_adam_dense_13_kernel_vIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_21IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_21AssignVariableOp(assignvariableop_21_adam_dense_13_bias_vIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_22IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_22AssignVariableOp*assignvariableop_22_adam_dense_12_kernel_vIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_23IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:?
AssignVariableOp_23AssignVariableOp(assignvariableop_23_adam_dense_12_bias_vIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype01
NoOpNoOp"/device:CPU:0*
_output_shapes
 ?
Identity_24Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_93^MutableHashTable_table_restore/LookupTableImportV2^NoOp"/device:CPU:0*
T0*
_output_shapes
: W
Identity_25IdentityIdentity_24:output:0^NoOp_1*
T0*
_output_shapes
: ?
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_93^MutableHashTable_table_restore/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 "#
identity_25Identity_25:output:0*G
_input_shapes6
4: : : : : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222*
AssignVariableOp_23AssignVariableOp_232(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_92h
2MutableHashTable_table_restore/LookupTableImportV22MutableHashTable_table_restore/LookupTableImportV2:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:)%
#
_class
loc:@MutableHashTable
?
n
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240839

inputs
identityW
Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :e
MaxMaxinputsMax/reduction_indices:output:0*
T0*(
_output_shapes
:??????????U
IdentityIdentityMax:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:?????????d?:T P
,
_output_shapes
:?????????d?
 
_user_specified_nameinputs
?	
d
E__inference_dropout_6_layer_call_and_return_conditional_losses_240225

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *   @d
dropout/MulMulinputsdropout/Const:output:0*
T0*'
_output_shapes
:?????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:?????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *   ??
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????o
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????i
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:?????????Y
IdentityIdentitydropout/Mul_1:z:0*
T0*'
_output_shapes
:?????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
__inference__initializer_2409199
5key_value_init219086_lookuptableimportv2_table_handle1
-key_value_init219086_lookuptableimportv2_keys3
/key_value_init219086_lookuptableimportv2_values	
identity??(key_value_init219086/LookupTableImportV2?
(key_value_init219086/LookupTableImportV2LookupTableImportV25key_value_init219086_lookuptableimportv2_table_handle-key_value_init219086_lookuptableimportv2_keys/key_value_init219086_lookuptableimportv2_values*	
Tin0*

Tout0	*
_output_shapes
 G
ConstConst*
_output_shapes
: *
dtype0*
value	B :L
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: q
NoOpNoOp)^key_value_init219086/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?N:?N2T
(key_value_init219086/LookupTableImportV2(key_value_init219086/LookupTableImportV2:!

_output_shapes	
:?N:!

_output_shapes	
:?N
?
G
__inference__creator_240929
identity: ??MutableHashTable?
MutableHashTableMutableHashTableV2*
_output_shapes
: *
	key_dtype0*
shared_nametable_208871*
value_dtype0	]
IdentityIdentityMutableHashTable:table_handle:0^NoOp*
T0*
_output_shapes
: Y
NoOpNoOp^MutableHashTable*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 2$
MutableHashTableMutableHashTable
?	
?
$__inference_signature_wrapper_240552
input_7
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:
?N?
	unknown_4:	?
	unknown_5:
	unknown_6:
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinput_7unknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*0
config_proto 

CPU

GPU2*0J 8? **
f%R#
!__inference__wrapped_model_240044o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_7:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
c
E__inference_dropout_6_layer_call_and_return_conditional_losses_240154

inputs

identity_1N
IdentityIdentityinputs*
T0*'
_output_shapes
:?????????[

Identity_1IdentityIdentity:output:0*
T0*'
_output_shapes
:?????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?s
?
C__inference_model_6_layer_call_and_return_conditional_losses_240672

inputsT
Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handleU
Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	1
-text_vectorization_6_string_lookup_12_equal_y4
0text_vectorization_6_string_lookup_12_selectv2_t	7
#embedding_6_embedding_lookup_240649:
?N?:
'dense_13_matmul_readvariableop_resource:	?6
(dense_13_biasadd_readvariableop_resource:9
'dense_12_matmul_readvariableop_resource:6
(dense_12_biasadd_readvariableop_resource:
identity??dense_12/BiasAdd/ReadVariableOp?dense_12/MatMul/ReadVariableOp?dense_13/BiasAdd/ReadVariableOp?dense_13/MatMul/ReadVariableOp?embedding_6/embedding_lookup?Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2`
 text_vectorization_6/StringLowerStringLowerinputs*'
_output_shapes
:??????????
'text_vectorization_6/StaticRegexReplaceStaticRegexReplace)text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
text_vectorization_6/SqueezeSqueeze0text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????g
&text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
.text_vectorization_6/StringSplit/StringSplitV2StringSplitV2%text_vectorization_6/Squeeze:output:0/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
4text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
6text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
6text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
.text_vectorization_6/StringSplit/strided_sliceStridedSlice8text_vectorization_6/StringSplit/StringSplitV2:indices:0=text_vectorization_6/StringSplit/strided_slice/stack:output:0?text_vectorization_6/StringSplit/strided_slice/stack_1:output:0?text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
6text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
8text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
8text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
0text_vectorization_6/StringSplit/strided_slice_1StridedSlice6text_vectorization_6/StringSplit/StringSplitV2:shape:0?text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
Wtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
etext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
dtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
btext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle7text_vectorization_6/StringSplit/StringSplitV2:values:0Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
+text_vectorization_6/string_lookup_12/EqualEqual7text_vectorization_6/StringSplit/StringSplitV2:values:0-text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/SelectV2SelectV2/text_vectorization_6/string_lookup_12/Equal:z:00text_vectorization_6_string_lookup_12_selectv2_tLtext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/IdentityIdentity7text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????s
1text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
)text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
8text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor2text_vectorization_6/RaggedToTensor/Const:output:07text_vectorization_6/string_lookup_12/Identity:output:0:text_vectorization_6/RaggedToTensor/default_value:output:09text_vectorization_6/StringSplit/strided_slice_1:output:07text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
embedding_6/embedding_lookupResourceGather#embedding_6_embedding_lookup_240649Atext_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0*
Tindices0	*6
_class,
*(loc:@embedding_6/embedding_lookup/240649*,
_output_shapes
:?????????d?*
dtype0?
%embedding_6/embedding_lookup/IdentityIdentity%embedding_6/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_6/embedding_lookup/240649*,
_output_shapes
:?????????d??
'embedding_6/embedding_lookup/Identity_1Identity.embedding_6/embedding_lookup/Identity:output:0*
T0*,
_output_shapes
:?????????d?n
,global_max_pooling1d_6/Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :?
global_max_pooling1d_6/MaxMax0embedding_6/embedding_lookup/Identity_1:output:05global_max_pooling1d_6/Max/reduction_indices:output:0*
T0*(
_output_shapes
:???????????
dense_13/MatMul/ReadVariableOpReadVariableOp'dense_13_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype0?
dense_13/MatMulMatMul#global_max_pooling1d_6/Max:output:0&dense_13/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
dense_13/BiasAdd/ReadVariableOpReadVariableOp(dense_13_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_13/BiasAddBiasAdddense_13/MatMul:product:0'dense_13/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????b
dense_13/ReluReludense_13/BiasAdd:output:0*
T0*'
_output_shapes
:?????????m
dropout_6/IdentityIdentitydense_13/Relu:activations:0*
T0*'
_output_shapes
:??????????
dense_12/MatMul/ReadVariableOpReadVariableOp'dense_12_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
dense_12/MatMulMatMuldropout_6/Identity:output:0&dense_12/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
dense_12/BiasAdd/ReadVariableOpReadVariableOp(dense_12_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_12/BiasAddBiasAdddense_12/MatMul:product:0'dense_12/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????h
dense_12/SigmoidSigmoiddense_12/BiasAdd:output:0*
T0*'
_output_shapes
:?????????c
IdentityIdentitydense_12/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp ^dense_12/BiasAdd/ReadVariableOp^dense_12/MatMul/ReadVariableOp ^dense_13/BiasAdd/ReadVariableOp^dense_13/MatMul/ReadVariableOp^embedding_6/embedding_lookupD^text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2B
dense_12/BiasAdd/ReadVariableOpdense_12/BiasAdd/ReadVariableOp2@
dense_12/MatMul/ReadVariableOpdense_12/MatMul/ReadVariableOp2B
dense_13/BiasAdd/ReadVariableOpdense_13/BiasAdd/ReadVariableOp2@
dense_13/MatMul/ReadVariableOpdense_13/MatMul/ReadVariableOp2<
embedding_6/embedding_lookupembedding_6/embedding_lookup2?
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?m
?
C__inference_model_6_layer_call_and_return_conditional_losses_240454
input_7T
Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handleU
Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	1
-text_vectorization_6_string_lookup_12_equal_y4
0text_vectorization_6_string_lookup_12_selectv2_t	&
embedding_6_240438:
?N?"
dense_13_240442:	?
dense_13_240444:!
dense_12_240448:
dense_12_240450:
identity?? dense_12/StatefulPartitionedCall? dense_13/StatefulPartitionedCall?#embedding_6/StatefulPartitionedCall?Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2a
 text_vectorization_6/StringLowerStringLowerinput_7*'
_output_shapes
:??????????
'text_vectorization_6/StaticRegexReplaceStaticRegexReplace)text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
text_vectorization_6/SqueezeSqueeze0text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????g
&text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
.text_vectorization_6/StringSplit/StringSplitV2StringSplitV2%text_vectorization_6/Squeeze:output:0/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
4text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
6text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
6text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
.text_vectorization_6/StringSplit/strided_sliceStridedSlice8text_vectorization_6/StringSplit/StringSplitV2:indices:0=text_vectorization_6/StringSplit/strided_slice/stack:output:0?text_vectorization_6/StringSplit/strided_slice/stack_1:output:0?text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
6text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
8text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
8text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
0text_vectorization_6/StringSplit/strided_slice_1StridedSlice6text_vectorization_6/StringSplit/StringSplitV2:shape:0?text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
Wtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
etext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
dtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
btext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle7text_vectorization_6/StringSplit/StringSplitV2:values:0Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
+text_vectorization_6/string_lookup_12/EqualEqual7text_vectorization_6/StringSplit/StringSplitV2:values:0-text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/SelectV2SelectV2/text_vectorization_6/string_lookup_12/Equal:z:00text_vectorization_6_string_lookup_12_selectv2_tLtext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/IdentityIdentity7text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????s
1text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
)text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
8text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor2text_vectorization_6/RaggedToTensor/Const:output:07text_vectorization_6/string_lookup_12/Identity:output:0:text_vectorization_6/RaggedToTensor/default_value:output:09text_vectorization_6/StringSplit/strided_slice_1:output:07text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
#embedding_6/StatefulPartitionedCallStatefulPartitionedCallAtext_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0embedding_6_240438*
Tin
2	*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:?????????d?*#
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *P
fKRI
G__inference_embedding_6_layer_call_and_return_conditional_losses_240121?
&global_max_pooling1d_6/PartitionedCallPartitionedCall,embedding_6/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *[
fVRT
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240130?
 dense_13/StatefulPartitionedCallStatefulPartitionedCall/global_max_pooling1d_6/PartitionedCall:output:0dense_13_240442dense_13_240444*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_13_layer_call_and_return_conditional_losses_240143?
dropout_6/PartitionedCallPartitionedCall)dense_13/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *N
fIRG
E__inference_dropout_6_layer_call_and_return_conditional_losses_240154?
 dense_12/StatefulPartitionedCallStatefulPartitionedCall"dropout_6/PartitionedCall:output:0dense_12_240448dense_12_240450*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_12_layer_call_and_return_conditional_losses_240167x
IdentityIdentity)dense_12/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp!^dense_12/StatefulPartitionedCall!^dense_13/StatefulPartitionedCall$^embedding_6/StatefulPartitionedCallD^text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_12/StatefulPartitionedCall dense_12/StatefulPartitionedCall2D
 dense_13/StatefulPartitionedCall dense_13/StatefulPartitionedCall2J
#embedding_6/StatefulPartitionedCall#embedding_6/StatefulPartitionedCall2?
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_7:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?z
?
C__inference_model_6_layer_call_and_return_conditional_losses_240753

inputsT
Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handleU
Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	1
-text_vectorization_6_string_lookup_12_equal_y4
0text_vectorization_6_string_lookup_12_selectv2_t	7
#embedding_6_embedding_lookup_240723:
?N?:
'dense_13_matmul_readvariableop_resource:	?6
(dense_13_biasadd_readvariableop_resource:9
'dense_12_matmul_readvariableop_resource:6
(dense_12_biasadd_readvariableop_resource:
identity??dense_12/BiasAdd/ReadVariableOp?dense_12/MatMul/ReadVariableOp?dense_13/BiasAdd/ReadVariableOp?dense_13/MatMul/ReadVariableOp?embedding_6/embedding_lookup?Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2`
 text_vectorization_6/StringLowerStringLowerinputs*'
_output_shapes
:??????????
'text_vectorization_6/StaticRegexReplaceStaticRegexReplace)text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
text_vectorization_6/SqueezeSqueeze0text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????g
&text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
.text_vectorization_6/StringSplit/StringSplitV2StringSplitV2%text_vectorization_6/Squeeze:output:0/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
4text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
6text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
6text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
.text_vectorization_6/StringSplit/strided_sliceStridedSlice8text_vectorization_6/StringSplit/StringSplitV2:indices:0=text_vectorization_6/StringSplit/strided_slice/stack:output:0?text_vectorization_6/StringSplit/strided_slice/stack_1:output:0?text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
6text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
8text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
8text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
0text_vectorization_6/StringSplit/strided_slice_1StridedSlice6text_vectorization_6/StringSplit/StringSplitV2:shape:0?text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
Wtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
etext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
dtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
btext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle7text_vectorization_6/StringSplit/StringSplitV2:values:0Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
+text_vectorization_6/string_lookup_12/EqualEqual7text_vectorization_6/StringSplit/StringSplitV2:values:0-text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/SelectV2SelectV2/text_vectorization_6/string_lookup_12/Equal:z:00text_vectorization_6_string_lookup_12_selectv2_tLtext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/IdentityIdentity7text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????s
1text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
)text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
8text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor2text_vectorization_6/RaggedToTensor/Const:output:07text_vectorization_6/string_lookup_12/Identity:output:0:text_vectorization_6/RaggedToTensor/default_value:output:09text_vectorization_6/StringSplit/strided_slice_1:output:07text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
embedding_6/embedding_lookupResourceGather#embedding_6_embedding_lookup_240723Atext_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0*
Tindices0	*6
_class,
*(loc:@embedding_6/embedding_lookup/240723*,
_output_shapes
:?????????d?*
dtype0?
%embedding_6/embedding_lookup/IdentityIdentity%embedding_6/embedding_lookup:output:0*
T0*6
_class,
*(loc:@embedding_6/embedding_lookup/240723*,
_output_shapes
:?????????d??
'embedding_6/embedding_lookup/Identity_1Identity.embedding_6/embedding_lookup/Identity:output:0*
T0*,
_output_shapes
:?????????d?n
,global_max_pooling1d_6/Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :?
global_max_pooling1d_6/MaxMax0embedding_6/embedding_lookup/Identity_1:output:05global_max_pooling1d_6/Max/reduction_indices:output:0*
T0*(
_output_shapes
:???????????
dense_13/MatMul/ReadVariableOpReadVariableOp'dense_13_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype0?
dense_13/MatMulMatMul#global_max_pooling1d_6/Max:output:0&dense_13/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
dense_13/BiasAdd/ReadVariableOpReadVariableOp(dense_13_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_13/BiasAddBiasAdddense_13/MatMul:product:0'dense_13/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????b
dense_13/ReluReludense_13/BiasAdd:output:0*
T0*'
_output_shapes
:?????????\
dropout_6/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *   @?
dropout_6/dropout/MulMuldense_13/Relu:activations:0 dropout_6/dropout/Const:output:0*
T0*'
_output_shapes
:?????????b
dropout_6/dropout/ShapeShapedense_13/Relu:activations:0*
T0*
_output_shapes
:?
.dropout_6/dropout/random_uniform/RandomUniformRandomUniform dropout_6/dropout/Shape:output:0*
T0*'
_output_shapes
:?????????*
dtype0e
 dropout_6/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *   ??
dropout_6/dropout/GreaterEqualGreaterEqual7dropout_6/dropout/random_uniform/RandomUniform:output:0)dropout_6/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:??????????
dropout_6/dropout/CastCast"dropout_6/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:??????????
dropout_6/dropout/Mul_1Muldropout_6/dropout/Mul:z:0dropout_6/dropout/Cast:y:0*
T0*'
_output_shapes
:??????????
dense_12/MatMul/ReadVariableOpReadVariableOp'dense_12_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
dense_12/MatMulMatMuldropout_6/dropout/Mul_1:z:0&dense_12/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
dense_12/BiasAdd/ReadVariableOpReadVariableOp(dense_12_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
dense_12/BiasAddBiasAdddense_12/MatMul:product:0'dense_12/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????h
dense_12/SigmoidSigmoiddense_12/BiasAdd:output:0*
T0*'
_output_shapes
:?????????c
IdentityIdentitydense_12/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp ^dense_12/BiasAdd/ReadVariableOp^dense_12/MatMul/ReadVariableOp ^dense_13/BiasAdd/ReadVariableOp^dense_13/MatMul/ReadVariableOp^embedding_6/embedding_lookupD^text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2B
dense_12/BiasAdd/ReadVariableOpdense_12/BiasAdd/ReadVariableOp2@
dense_12/MatMul/ReadVariableOpdense_12/MatMul/ReadVariableOp2B
dense_13/BiasAdd/ReadVariableOpdense_13/BiasAdd/ReadVariableOp2@
dense_13/MatMul/ReadVariableOpdense_13/MatMul/ReadVariableOp2<
embedding_6/embedding_lookupembedding_6/embedding_lookup2?
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
/
__inference__initializer_240934
identityG
ConstConst*
_output_shapes
: *
dtype0*
value	B :E
IdentityIdentityConst:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
?
__inference_save_fn_240958
checkpoint_keyP
Lmutablehashtable_lookup_table_export_values_lookuptableexportv2_table_handle
identity

identity_1

identity_2

identity_3

identity_4

identity_5	???MutableHashTable_lookup_table_export_values/LookupTableExportV2?
?MutableHashTable_lookup_table_export_values/LookupTableExportV2LookupTableExportV2Lmutablehashtable_lookup_table_export_values_lookuptableexportv2_table_handle",/job:localhost/replica:0/task:0/device:CPU:0*
Tkeys0*
Tvalues0	*
_output_shapes

::K
add/yConst*
_output_shapes
: *
dtype0*
valueB B-keysK
addAddcheckpoint_keyadd/y:output:0*
T0*
_output_shapes
: O
add_1/yConst*
_output_shapes
: *
dtype0*
valueB B-valuesO
add_1Addcheckpoint_keyadd_1/y:output:0*
T0*
_output_shapes
: E
IdentityIdentityadd:z:0^NoOp*
T0*
_output_shapes
: F
ConstConst*
_output_shapes
: *
dtype0*
valueB B N

Identity_1IdentityConst:output:0^NoOp*
T0*
_output_shapes
: ?

Identity_2IdentityFMutableHashTable_lookup_table_export_values/LookupTableExportV2:keys:0^NoOp*
T0*
_output_shapes
:I

Identity_3Identity	add_1:z:0^NoOp*
T0*
_output_shapes
: H
Const_1Const*
_output_shapes
: *
dtype0*
valueB B P

Identity_4IdentityConst_1:output:0^NoOp*
T0*
_output_shapes
: ?

Identity_5IdentityHMutableHashTable_lookup_table_export_values/LookupTableExportV2:values:0^NoOp*
T0	*
_output_shapes
:?
NoOpNoOp@^MutableHashTable_lookup_table_export_values/LookupTableExportV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*
_input_shapes
: : 2?
?MutableHashTable_lookup_table_export_values/LookupTableExportV2?MutableHashTable_lookup_table_export_values/LookupTableExportV2:F B

_output_shapes
: 
(
_user_specified_namecheckpoint_key
?
?
__inference_restore_fn_240966
restored_tensors_0
restored_tensors_1	C
?mutablehashtable_table_restore_lookuptableimportv2_table_handle
identity??2MutableHashTable_table_restore/LookupTableImportV2?
2MutableHashTable_table_restore/LookupTableImportV2LookupTableImportV2?mutablehashtable_table_restore_lookuptableimportv2_table_handlerestored_tensors_0restored_tensors_1",/job:localhost/replica:0/task:0/device:CPU:0*	
Tin0*

Tout0	*
_output_shapes
 G
ConstConst*
_output_shapes
: *
dtype0*
value	B :L
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: {
NoOpNoOp3^MutableHashTable_table_restore/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes

::: 2h
2MutableHashTable_table_restore/LookupTableImportV22MutableHashTable_table_restore/LookupTableImportV2:L H

_output_shapes
:
,
_user_specified_namerestored_tensors_0:LH

_output_shapes
:
,
_user_specified_namerestored_tensors_1
?
?
__inference_<lambda>_2409819
5key_value_init219086_lookuptableimportv2_table_handle1
-key_value_init219086_lookuptableimportv2_keys3
/key_value_init219086_lookuptableimportv2_values	
identity??(key_value_init219086/LookupTableImportV2?
(key_value_init219086/LookupTableImportV2LookupTableImportV25key_value_init219086_lookuptableimportv2_table_handle-key_value_init219086_lookuptableimportv2_keys/key_value_init219086_lookuptableimportv2_values*	
Tin0*

Tout0	*
_output_shapes
 J
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??L
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: q
NoOpNoOp)^key_value_init219086/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?N:?N2T
(key_value_init219086/LookupTableImportV2(key_value_init219086/LookupTableImportV2:!

_output_shapes	
:?N:!

_output_shapes	
:?N
?
?
,__inference_embedding_6_layer_call_fn_240808

inputs	
unknown:
?N?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown*
Tin
2	*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:?????????d?*#
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *P
fKRI
G__inference_embedding_6_layer_call_and_return_conditional_losses_240121t
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*,
_output_shapes
:?????????d?`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????d: 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????d
 
_user_specified_nameinputs
?n
?
C__inference_model_6_layer_call_and_return_conditional_losses_240521
input_7T
Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handleU
Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	1
-text_vectorization_6_string_lookup_12_equal_y4
0text_vectorization_6_string_lookup_12_selectv2_t	&
embedding_6_240505:
?N?"
dense_13_240509:	?
dense_13_240511:!
dense_12_240515:
dense_12_240517:
identity?? dense_12/StatefulPartitionedCall? dense_13/StatefulPartitionedCall?!dropout_6/StatefulPartitionedCall?#embedding_6/StatefulPartitionedCall?Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2a
 text_vectorization_6/StringLowerStringLowerinput_7*'
_output_shapes
:??????????
'text_vectorization_6/StaticRegexReplaceStaticRegexReplace)text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
text_vectorization_6/SqueezeSqueeze0text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????g
&text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
.text_vectorization_6/StringSplit/StringSplitV2StringSplitV2%text_vectorization_6/Squeeze:output:0/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
4text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
6text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
6text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
.text_vectorization_6/StringSplit/strided_sliceStridedSlice8text_vectorization_6/StringSplit/StringSplitV2:indices:0=text_vectorization_6/StringSplit/strided_slice/stack:output:0?text_vectorization_6/StringSplit/strided_slice/stack_1:output:0?text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
6text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
8text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
8text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
0text_vectorization_6/StringSplit/strided_slice_1StridedSlice6text_vectorization_6/StringSplit/StringSplitV2:shape:0?text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
Wtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
etext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
dtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
btext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle7text_vectorization_6/StringSplit/StringSplitV2:values:0Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
+text_vectorization_6/string_lookup_12/EqualEqual7text_vectorization_6/StringSplit/StringSplitV2:values:0-text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/SelectV2SelectV2/text_vectorization_6/string_lookup_12/Equal:z:00text_vectorization_6_string_lookup_12_selectv2_tLtext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/IdentityIdentity7text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????s
1text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
)text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
8text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor2text_vectorization_6/RaggedToTensor/Const:output:07text_vectorization_6/string_lookup_12/Identity:output:0:text_vectorization_6/RaggedToTensor/default_value:output:09text_vectorization_6/StringSplit/strided_slice_1:output:07text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
#embedding_6/StatefulPartitionedCallStatefulPartitionedCallAtext_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0embedding_6_240505*
Tin
2	*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:?????????d?*#
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *P
fKRI
G__inference_embedding_6_layer_call_and_return_conditional_losses_240121?
&global_max_pooling1d_6/PartitionedCallPartitionedCall,embedding_6/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *[
fVRT
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240130?
 dense_13/StatefulPartitionedCallStatefulPartitionedCall/global_max_pooling1d_6/PartitionedCall:output:0dense_13_240509dense_13_240511*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_13_layer_call_and_return_conditional_losses_240143?
!dropout_6/StatefulPartitionedCallStatefulPartitionedCall)dense_13/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *N
fIRG
E__inference_dropout_6_layer_call_and_return_conditional_losses_240225?
 dense_12/StatefulPartitionedCallStatefulPartitionedCall*dropout_6/StatefulPartitionedCall:output:0dense_12_240515dense_12_240517*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_12_layer_call_and_return_conditional_losses_240167x
IdentityIdentity)dense_12/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp!^dense_12/StatefulPartitionedCall!^dense_13/StatefulPartitionedCall"^dropout_6/StatefulPartitionedCall$^embedding_6/StatefulPartitionedCallD^text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_12/StatefulPartitionedCall dense_12/StatefulPartitionedCall2D
 dense_13/StatefulPartitionedCall dense_13/StatefulPartitionedCall2F
!dropout_6/StatefulPartitionedCall!dropout_6/StatefulPartitionedCall2J
#embedding_6/StatefulPartitionedCall#embedding_6/StatefulPartitionedCall2?
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_7:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?	
d
E__inference_dropout_6_layer_call_and_return_conditional_losses_240886

inputs
identity?R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *   @d
dropout/MulMulinputsdropout/Const:output:0*
T0*'
_output_shapes
:?????????C
dropout/ShapeShapeinputs*
T0*
_output_shapes
:?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:?????????*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *   ??
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????o
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????i
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:?????????Y
IdentityIdentitydropout/Mul_1:z:0*
T0*'
_output_shapes
:?????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
D__inference_dense_13_layer_call_and_return_conditional_losses_240859

inputs1
matmul_readvariableop_resource:	?-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????P
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????a
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?

?
(__inference_model_6_layer_call_fn_240195
input_7
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:
?N?
	unknown_4:	?
	unknown_5:
	unknown_6:
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinput_7unknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*0
config_proto 

CPU

GPU2*0J 8? *L
fGRE
C__inference_model_6_layer_call_and_return_conditional_losses_240174o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_7:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
-
__inference__destroyer_240924
identityG
ConstConst*
_output_shapes
: *
dtype0*
value	B :E
IdentityIdentityConst:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?;
?
__inference__traced_save_241095
file_prefix5
1savev2_embedding_6_embeddings_read_readvariableop.
*savev2_dense_13_kernel_read_readvariableop,
(savev2_dense_13_bias_read_readvariableop.
*savev2_dense_12_kernel_read_readvariableop,
(savev2_dense_12_bias_read_readvariableop(
$savev2_adam_iter_read_readvariableop	*
&savev2_adam_beta_1_read_readvariableop*
&savev2_adam_beta_2_read_readvariableop)
%savev2_adam_decay_read_readvariableop1
-savev2_adam_learning_rate_read_readvariableopJ
Fsavev2_mutablehashtable_lookup_table_export_values_lookuptableexportv2L
Hsavev2_mutablehashtable_lookup_table_export_values_lookuptableexportv2_1	$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop<
8savev2_adam_embedding_6_embeddings_m_read_readvariableop5
1savev2_adam_dense_13_kernel_m_read_readvariableop3
/savev2_adam_dense_13_bias_m_read_readvariableop5
1savev2_adam_dense_12_kernel_m_read_readvariableop3
/savev2_adam_dense_12_bias_m_read_readvariableop<
8savev2_adam_embedding_6_embeddings_v_read_readvariableop5
1savev2_adam_dense_13_kernel_v_read_readvariableop3
/savev2_adam_dense_13_bias_v_read_readvariableop5
1savev2_adam_dense_12_kernel_v_read_readvariableop3
/savev2_adam_dense_12_bias_v_read_readvariableop
savev2_const_6

identity_1??MergeV2Checkpointsw
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*Z
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.parta
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: f

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: L

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :f
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : ?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: ?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value?B?B:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEBFlayer_with_weights-0/_lookup_layer/token_counts/.ATTRIBUTES/table-keysBHlayer_with_weights-0/_lookup_layer/token_counts/.ATTRIBUTES/table-valuesB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBVlayer_with_weights-1/embeddings/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*I
value@B>B B B B B B B B B B B B B B B B B B B B B B B B B B B ?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:01savev2_embedding_6_embeddings_read_readvariableop*savev2_dense_13_kernel_read_readvariableop(savev2_dense_13_bias_read_readvariableop*savev2_dense_12_kernel_read_readvariableop(savev2_dense_12_bias_read_readvariableop$savev2_adam_iter_read_readvariableop&savev2_adam_beta_1_read_readvariableop&savev2_adam_beta_2_read_readvariableop%savev2_adam_decay_read_readvariableop-savev2_adam_learning_rate_read_readvariableopFsavev2_mutablehashtable_lookup_table_export_values_lookuptableexportv2Hsavev2_mutablehashtable_lookup_table_export_values_lookuptableexportv2_1 savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableop8savev2_adam_embedding_6_embeddings_m_read_readvariableop1savev2_adam_dense_13_kernel_m_read_readvariableop/savev2_adam_dense_13_bias_m_read_readvariableop1savev2_adam_dense_12_kernel_m_read_readvariableop/savev2_adam_dense_12_bias_m_read_readvariableop8savev2_adam_embedding_6_embeddings_v_read_readvariableop1savev2_adam_dense_13_kernel_v_read_readvariableop/savev2_adam_dense_13_bias_v_read_readvariableop1savev2_adam_dense_12_kernel_v_read_readvariableop/savev2_adam_dense_12_bias_v_read_readvariableopsavev2_const_6"/device:CPU:0*
_output_shapes
 *)
dtypes
2		?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 f
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: Q

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: [
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 "!

identity_1Identity_1:output:0*?
_input_shapes?
?: :
?N?:	?:::: : : : : ::: : : : :
?N?:	?::::
?N?:	?:::: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:&"
 
_output_shapes
:
?N?:%!

_output_shapes
:	?: 

_output_shapes
::$ 

_output_shapes

:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: :


_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :&"
 
_output_shapes
:
?N?:%!

_output_shapes
:	?: 

_output_shapes
::$ 

_output_shapes

:: 

_output_shapes
::&"
 
_output_shapes
:
?N?:%!

_output_shapes
:	?: 

_output_shapes
::$ 

_output_shapes

:: 

_output_shapes
::

_output_shapes
: 
?

?
(__inference_model_6_layer_call_fn_240575

inputs
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:
?N?
	unknown_4:	?
	unknown_5:
	unknown_6:
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*0
config_proto 

CPU

GPU2*0J 8? *L
fGRE
C__inference_model_6_layer_call_and_return_conditional_losses_240174o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
n
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240833

inputs
identityW
Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :m
MaxMaxinputsMax/reduction_indices:output:0*
T0*0
_output_shapes
:??????????????????]
IdentityIdentityMax:output:0*
T0*0
_output_shapes
:??????????????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*<
_input_shapes+
):'???????????????????????????:e a
=
_output_shapes+
):'???????????????????????????
 
_user_specified_nameinputs
?
?
G__inference_embedding_6_layer_call_and_return_conditional_losses_240817

inputs	+
embedding_lookup_240811:
?N?
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_240811inputs*
Tindices0	**
_class 
loc:@embedding_lookup/240811*,
_output_shapes
:?????????d?*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/240811*,
_output_shapes
:?????????d??
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*,
_output_shapes
:?????????d?x
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*,
_output_shapes
:?????????d?Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????d: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????d
 
_user_specified_nameinputs
?
?
)__inference_dense_13_layer_call_fn_240848

inputs
unknown:	?
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_13_layer_call_and_return_conditional_losses_240143o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:??????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:??????????
 
_user_specified_nameinputs
?
n
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240054

inputs
identityW
Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :m
MaxMaxinputsMax/reduction_indices:output:0*
T0*0
_output_shapes
:??????????????????]
IdentityIdentityMax:output:0*
T0*0
_output_shapes
:??????????????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*<
_input_shapes+
):'???????????????????????????:e a
=
_output_shapes+
):'???????????????????????????
 
_user_specified_nameinputs
?
F
*__inference_dropout_6_layer_call_fn_240864

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *N
fIRG
E__inference_dropout_6_layer_call_and_return_conditional_losses_240154`
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
(__inference_model_6_layer_call_fn_240598

inputs
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:
?N?
	unknown_4:	?
	unknown_5:
	unknown_6:
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*0
config_proto 

CPU

GPU2*0J 8? *L
fGRE
C__inference_model_6_layer_call_and_return_conditional_losses_240343o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
;
__inference__creator_240911
identity??
hash_tablen

hash_tableHashTableV2*
_output_shapes
: *
	key_dtype0*
shared_name219087*
value_dtype0	W
IdentityIdentityhash_table:table_handle:0^NoOp*
T0*
_output_shapes
: S
NoOpNoOp^hash_table*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 2

hash_table
hash_table
?n
?
C__inference_model_6_layer_call_and_return_conditional_losses_240343

inputsT
Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handleU
Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	1
-text_vectorization_6_string_lookup_12_equal_y4
0text_vectorization_6_string_lookup_12_selectv2_t	&
embedding_6_240327:
?N?"
dense_13_240331:	?
dense_13_240333:!
dense_12_240337:
dense_12_240339:
identity?? dense_12/StatefulPartitionedCall? dense_13/StatefulPartitionedCall?!dropout_6/StatefulPartitionedCall?#embedding_6/StatefulPartitionedCall?Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2`
 text_vectorization_6/StringLowerStringLowerinputs*'
_output_shapes
:??????????
'text_vectorization_6/StaticRegexReplaceStaticRegexReplace)text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
text_vectorization_6/SqueezeSqueeze0text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????g
&text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
.text_vectorization_6/StringSplit/StringSplitV2StringSplitV2%text_vectorization_6/Squeeze:output:0/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
4text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
6text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
6text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
.text_vectorization_6/StringSplit/strided_sliceStridedSlice8text_vectorization_6/StringSplit/StringSplitV2:indices:0=text_vectorization_6/StringSplit/strided_slice/stack:output:0?text_vectorization_6/StringSplit/strided_slice/stack_1:output:0?text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
6text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
8text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
8text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
0text_vectorization_6/StringSplit/strided_slice_1StridedSlice6text_vectorization_6/StringSplit/StringSplitV2:shape:0?text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
Wtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
etext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
`text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
atext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
ctext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
dtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
btext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
^text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
Ytext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Ptext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle7text_vectorization_6/StringSplit/StringSplitV2:values:0Qtext_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
+text_vectorization_6/string_lookup_12/EqualEqual7text_vectorization_6/StringSplit/StringSplitV2:values:0-text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/SelectV2SelectV2/text_vectorization_6/string_lookup_12/Equal:z:00text_vectorization_6_string_lookup_12_selectv2_tLtext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
.text_vectorization_6/string_lookup_12/IdentityIdentity7text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????s
1text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
)text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
8text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor2text_vectorization_6/RaggedToTensor/Const:output:07text_vectorization_6/string_lookup_12/Identity:output:0:text_vectorization_6/RaggedToTensor/default_value:output:09text_vectorization_6/StringSplit/strided_slice_1:output:07text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
#embedding_6/StatefulPartitionedCallStatefulPartitionedCallAtext_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0embedding_6_240327*
Tin
2	*
Tout
2*
_collective_manager_ids
 *,
_output_shapes
:?????????d?*#
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *P
fKRI
G__inference_embedding_6_layer_call_and_return_conditional_losses_240121?
&global_max_pooling1d_6/PartitionedCallPartitionedCall,embedding_6/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *[
fVRT
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240130?
 dense_13/StatefulPartitionedCallStatefulPartitionedCall/global_max_pooling1d_6/PartitionedCall:output:0dense_13_240331dense_13_240333*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_13_layer_call_and_return_conditional_losses_240143?
!dropout_6/StatefulPartitionedCallStatefulPartitionedCall)dense_13/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *N
fIRG
E__inference_dropout_6_layer_call_and_return_conditional_losses_240225?
 dense_12/StatefulPartitionedCallStatefulPartitionedCall*dropout_6/StatefulPartitionedCall:output:0dense_12_240337dense_12_240339*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_12_layer_call_and_return_conditional_losses_240167x
IdentityIdentity)dense_12/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp!^dense_12/StatefulPartitionedCall!^dense_13/StatefulPartitionedCall"^dropout_6/StatefulPartitionedCall$^embedding_6/StatefulPartitionedCallD^text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_12/StatefulPartitionedCall dense_12/StatefulPartitionedCall2D
 dense_13/StatefulPartitionedCall dense_13/StatefulPartitionedCall2F
!dropout_6/StatefulPartitionedCall!dropout_6/StatefulPartitionedCall2J
#embedding_6/StatefulPartitionedCall#embedding_6/StatefulPartitionedCall2?
Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Ctext_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
S
7__inference_global_max_pooling1d_6_layer_call_fn_240822

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *0
_output_shapes
:??????????????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *[
fVRT
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240054i
IdentityIdentityPartitionedCall:output:0*
T0*0
_output_shapes
:??????????????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*<
_input_shapes+
):'???????????????????????????:e a
=
_output_shapes+
):'???????????????????????????
 
_user_specified_nameinputs
?
+
__inference_<lambda>_240986
identityJ
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??E
IdentityIdentityConst:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?}
?
!__inference__wrapped_model_240044
input_7\
Xmodel_6_text_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle]
Ymodel_6_text_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value	9
5model_6_text_vectorization_6_string_lookup_12_equal_y<
8model_6_text_vectorization_6_string_lookup_12_selectv2_t	?
+model_6_embedding_6_embedding_lookup_240021:
?N?B
/model_6_dense_13_matmul_readvariableop_resource:	?>
0model_6_dense_13_biasadd_readvariableop_resource:A
/model_6_dense_12_matmul_readvariableop_resource:>
0model_6_dense_12_biasadd_readvariableop_resource:
identity??'model_6/dense_12/BiasAdd/ReadVariableOp?&model_6/dense_12/MatMul/ReadVariableOp?'model_6/dense_13/BiasAdd/ReadVariableOp?&model_6/dense_13/MatMul/ReadVariableOp?$model_6/embedding_6/embedding_lookup?Kmodel_6/text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2i
(model_6/text_vectorization_6/StringLowerStringLowerinput_7*'
_output_shapes
:??????????
/model_6/text_vectorization_6/StaticRegexReplaceStaticRegexReplace1model_6/text_vectorization_6/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite ?
$model_6/text_vectorization_6/SqueezeSqueeze8model_6/text_vectorization_6/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????o
.model_6/text_vectorization_6/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B ?
6model_6/text_vectorization_6/StringSplit/StringSplitV2StringSplitV2-model_6/text_vectorization_6/Squeeze:output:07model_6/text_vectorization_6/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:?
<model_6/text_vectorization_6/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        ?
>model_6/text_vectorization_6/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       ?
>model_6/text_vectorization_6/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      ?
6model_6/text_vectorization_6/StringSplit/strided_sliceStridedSlice@model_6/text_vectorization_6/StringSplit/StringSplitV2:indices:0Emodel_6/text_vectorization_6/StringSplit/strided_slice/stack:output:0Gmodel_6/text_vectorization_6/StringSplit/strided_slice/stack_1:output:0Gmodel_6/text_vectorization_6/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask?
>model_6/text_vectorization_6/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: ?
@model_6/text_vectorization_6/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:?
@model_6/text_vectorization_6/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:?
8model_6/text_vectorization_6/StringSplit/strided_slice_1StridedSlice>model_6/text_vectorization_6/StringSplit/StringSplitV2:shape:0Gmodel_6/text_vectorization_6/StringSplit/strided_slice_1/stack:output:0Imodel_6/text_vectorization_6/StringSplit/strided_slice_1/stack_1:output:0Imodel_6/text_vectorization_6/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask?
_model_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast?model_6/text_vectorization_6/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:??????????
amodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1CastAmodel_6/text_vectorization_6/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: ?
imodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShapecmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:?
imodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: ?
hmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdrmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0rmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: ?
mmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : ?
kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreaterqmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0vmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: ?
hmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastomodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: ?
kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: ?
gmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMaxcmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0tmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: ?
imodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :?
gmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2pmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0rmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: ?
gmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMullmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: ?
kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximumemodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: ?
kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimumemodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0omodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: ?
kmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 ?
lmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincountcmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0omodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0tmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:??????????
fmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
amodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumsmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0omodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:??????????
jmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R ?
fmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : ?
amodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2smodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0gmodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0omodel_6/text_vectorization_6/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:??????????
Kmodel_6/text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2LookupTableFindV2Xmodel_6_text_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_table_handle?model_6/text_vectorization_6/StringSplit/StringSplitV2:values:0Ymodel_6_text_vectorization_6_string_lookup_12_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:??????????
3model_6/text_vectorization_6/string_lookup_12/EqualEqual?model_6/text_vectorization_6/StringSplit/StringSplitV2:values:05model_6_text_vectorization_6_string_lookup_12_equal_y*
T0*#
_output_shapes
:??????????
6model_6/text_vectorization_6/string_lookup_12/SelectV2SelectV27model_6/text_vectorization_6/string_lookup_12/Equal:z:08model_6_text_vectorization_6_string_lookup_12_selectv2_tTmodel_6/text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:??????????
6model_6/text_vectorization_6/string_lookup_12/IdentityIdentity?model_6/text_vectorization_6/string_lookup_12/SelectV2:output:0*
T0	*#
_output_shapes
:?????????{
9model_6/text_vectorization_6/RaggedToTensor/default_valueConst*
_output_shapes
: *
dtype0	*
value	B	 R ?
1model_6/text_vectorization_6/RaggedToTensor/ConstConst*
_output_shapes
:*
dtype0	*%
valueB	"????????d       ?
@model_6/text_vectorization_6/RaggedToTensor/RaggedTensorToTensorRaggedTensorToTensor:model_6/text_vectorization_6/RaggedToTensor/Const:output:0?model_6/text_vectorization_6/string_lookup_12/Identity:output:0Bmodel_6/text_vectorization_6/RaggedToTensor/default_value:output:0Amodel_6/text_vectorization_6/StringSplit/strided_slice_1:output:0?model_6/text_vectorization_6/StringSplit/strided_slice:output:0*
T0	*
Tindex0	*
Tshape0	*'
_output_shapes
:?????????d*
num_row_partition_tensors*7
row_partition_types 
FIRST_DIM_SIZEVALUE_ROWIDS?
$model_6/embedding_6/embedding_lookupResourceGather+model_6_embedding_6_embedding_lookup_240021Imodel_6/text_vectorization_6/RaggedToTensor/RaggedTensorToTensor:result:0*
Tindices0	*>
_class4
20loc:@model_6/embedding_6/embedding_lookup/240021*,
_output_shapes
:?????????d?*
dtype0?
-model_6/embedding_6/embedding_lookup/IdentityIdentity-model_6/embedding_6/embedding_lookup:output:0*
T0*>
_class4
20loc:@model_6/embedding_6/embedding_lookup/240021*,
_output_shapes
:?????????d??
/model_6/embedding_6/embedding_lookup/Identity_1Identity6model_6/embedding_6/embedding_lookup/Identity:output:0*
T0*,
_output_shapes
:?????????d?v
4model_6/global_max_pooling1d_6/Max/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :?
"model_6/global_max_pooling1d_6/MaxMax8model_6/embedding_6/embedding_lookup/Identity_1:output:0=model_6/global_max_pooling1d_6/Max/reduction_indices:output:0*
T0*(
_output_shapes
:???????????
&model_6/dense_13/MatMul/ReadVariableOpReadVariableOp/model_6_dense_13_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype0?
model_6/dense_13/MatMulMatMul+model_6/global_max_pooling1d_6/Max:output:0.model_6/dense_13/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
'model_6/dense_13/BiasAdd/ReadVariableOpReadVariableOp0model_6_dense_13_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
model_6/dense_13/BiasAddBiasAdd!model_6/dense_13/MatMul:product:0/model_6/dense_13/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
model_6/dense_13/ReluRelu!model_6/dense_13/BiasAdd:output:0*
T0*'
_output_shapes
:?????????}
model_6/dropout_6/IdentityIdentity#model_6/dense_13/Relu:activations:0*
T0*'
_output_shapes
:??????????
&model_6/dense_12/MatMul/ReadVariableOpReadVariableOp/model_6_dense_12_matmul_readvariableop_resource*
_output_shapes

:*
dtype0?
model_6/dense_12/MatMulMatMul#model_6/dropout_6/Identity:output:0.model_6/dense_12/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:??????????
'model_6/dense_12/BiasAdd/ReadVariableOpReadVariableOp0model_6_dense_12_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0?
model_6/dense_12/BiasAddBiasAdd!model_6/dense_12/MatMul:product:0/model_6/dense_12/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????x
model_6/dense_12/SigmoidSigmoid!model_6/dense_12/BiasAdd:output:0*
T0*'
_output_shapes
:?????????k
IdentityIdentitymodel_6/dense_12/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:??????????
NoOpNoOp(^model_6/dense_12/BiasAdd/ReadVariableOp'^model_6/dense_12/MatMul/ReadVariableOp(^model_6/dense_13/BiasAdd/ReadVariableOp'^model_6/dense_13/MatMul/ReadVariableOp%^model_6/embedding_6/embedding_lookupL^model_6/text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2R
'model_6/dense_12/BiasAdd/ReadVariableOp'model_6/dense_12/BiasAdd/ReadVariableOp2P
&model_6/dense_12/MatMul/ReadVariableOp&model_6/dense_12/MatMul/ReadVariableOp2R
'model_6/dense_13/BiasAdd/ReadVariableOp'model_6/dense_13/BiasAdd/ReadVariableOp2P
&model_6/dense_13/MatMul/ReadVariableOp&model_6/dense_13/MatMul/ReadVariableOp2L
$model_6/embedding_6/embedding_lookup$model_6/embedding_6/embedding_lookup2?
Kmodel_6/text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2Kmodel_6/text_vectorization_6/string_lookup_12/None_Lookup/LookupTableFindV2:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_7:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?

?
D__inference_dense_12_layer_call_and_return_conditional_losses_240906

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????V
SigmoidSigmoidBiasAdd:output:0*
T0*'
_output_shapes
:?????????Z
IdentityIdentitySigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
S
7__inference_global_max_pooling1d_6_layer_call_fn_240827

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:??????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *[
fVRT
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240130a
IdentityIdentityPartitionedCall:output:0*
T0*(
_output_shapes
:??????????"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:?????????d?:T P
,
_output_shapes
:?????????d?
 
_user_specified_nameinputs
?
?
G__inference_embedding_6_layer_call_and_return_conditional_losses_240121

inputs	+
embedding_lookup_240115:
?N?
identity??embedding_lookup?
embedding_lookupResourceGatherembedding_lookup_240115inputs*
Tindices0	**
_class 
loc:@embedding_lookup/240115*,
_output_shapes
:?????????d?*
dtype0?
embedding_lookup/IdentityIdentityembedding_lookup:output:0*
T0**
_class 
loc:@embedding_lookup/240115*,
_output_shapes
:?????????d??
embedding_lookup/Identity_1Identity"embedding_lookup/Identity:output:0*
T0*,
_output_shapes
:?????????d?x
IdentityIdentity$embedding_lookup/Identity_1:output:0^NoOp*
T0*,
_output_shapes
:?????????d?Y
NoOpNoOp^embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*(
_input_shapes
:?????????d: 2$
embedding_lookupembedding_lookup:O K
'
_output_shapes
:?????????d
 
_user_specified_nameinputs
?
c
E__inference_dropout_6_layer_call_and_return_conditional_losses_240874

inputs

identity_1N
IdentityIdentityinputs*
T0*'
_output_shapes
:?????????[

Identity_1IdentityIdentity:output:0*
T0*'
_output_shapes
:?????????"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
c
*__inference_dropout_6_layer_call_fn_240869

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8? *N
fIRG
E__inference_dropout_6_layer_call_and_return_conditional_losses_240225o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
)__inference_dense_12_layer_call_fn_240895

inputs
unknown:
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8? *M
fHRF
D__inference_dense_12_layer_call_and_return_conditional_losses_240167o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
D__inference_dense_12_layer_call_and_return_conditional_losses_240167

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????V
SigmoidSigmoidBiasAdd:output:0*
T0*'
_output_shapes
:?????????Z
IdentityIdentitySigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs"?L
saver_filename:0StatefulPartitionedCall_2:0StatefulPartitionedCall_38"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
;
input_70
serving_default_input_7:0?????????>
dense_122
StatefulPartitionedCall_1:0?????????tensorflow/serving/predict:??
?
layer-0
layer_with_weights-0
layer-1
layer_with_weights-1
layer-2
layer-3
layer_with_weights-2
layer-4
layer-5
layer_with_weights-3
layer-6
	optimizer
		variables

trainable_variables
regularization_losses
	keras_api

signatures
e__call__
*f&call_and_return_all_conditional_losses
g_default_save_signature"
_tf_keras_network
"
_tf_keras_input_layer
P
_lookup_layer
	keras_api
h_adapt_function"
_tf_keras_layer
?

embeddings
	variables
trainable_variables
regularization_losses
	keras_api
i__call__
*j&call_and_return_all_conditional_losses"
_tf_keras_layer
?
	variables
trainable_variables
regularization_losses
	keras_api
k__call__
*l&call_and_return_all_conditional_losses"
_tf_keras_layer
?

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
m__call__
*n&call_and_return_all_conditional_losses"
_tf_keras_layer
?
	variables
 trainable_variables
!regularization_losses
"	keras_api
o__call__
*p&call_and_return_all_conditional_losses"
_tf_keras_layer
?

#kernel
$bias
%	variables
&trainable_variables
'regularization_losses
(	keras_api
q__call__
*r&call_and_return_all_conditional_losses"
_tf_keras_layer
?
)iter

*beta_1

+beta_2
	,decay
-learning_ratem[m\m]#m^$m_v`vavb#vc$vd"
	optimizer
C
1
2
3
#4
$5"
trackable_list_wrapper
C
0
1
2
#3
$4"
trackable_list_wrapper
 "
trackable_list_wrapper
?
.non_trainable_variables

/layers
0metrics
1layer_regularization_losses
2layer_metrics
		variables

trainable_variables
regularization_losses
e__call__
g_default_save_signature
*f&call_and_return_all_conditional_losses
&f"call_and_return_conditional_losses"
_generic_user_object
,
sserving_default"
signature_map
L
3lookup_table
4token_counts
5	keras_api"
_tf_keras_layer
"
_generic_user_object
*:(
?N?2embedding_6/embeddings
'
0"
trackable_list_wrapper
'
0"
trackable_list_wrapper
 "
trackable_list_wrapper
?
6non_trainable_variables

7layers
8metrics
9layer_regularization_losses
:layer_metrics
	variables
trainable_variables
regularization_losses
i__call__
*j&call_and_return_all_conditional_losses
&j"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
;non_trainable_variables

<layers
=metrics
>layer_regularization_losses
?layer_metrics
	variables
trainable_variables
regularization_losses
k__call__
*l&call_and_return_all_conditional_losses
&l"call_and_return_conditional_losses"
_generic_user_object
": 	?2dense_13/kernel
:2dense_13/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
@non_trainable_variables

Alayers
Bmetrics
Clayer_regularization_losses
Dlayer_metrics
	variables
trainable_variables
regularization_losses
m__call__
*n&call_and_return_all_conditional_losses
&n"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
Enon_trainable_variables

Flayers
Gmetrics
Hlayer_regularization_losses
Ilayer_metrics
	variables
 trainable_variables
!regularization_losses
o__call__
*p&call_and_return_all_conditional_losses
&p"call_and_return_conditional_losses"
_generic_user_object
!:2dense_12/kernel
:2dense_12/bias
.
#0
$1"
trackable_list_wrapper
.
#0
$1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Jnon_trainable_variables

Klayers
Lmetrics
Mlayer_regularization_losses
Nlayer_metrics
%	variables
&trainable_variables
'regularization_losses
q__call__
*r&call_and_return_all_conditional_losses
&r"call_and_return_conditional_losses"
_generic_user_object
:	 (2	Adam/iter
: (2Adam/beta_1
: (2Adam/beta_2
: (2
Adam/decay
: (2Adam/learning_rate
 "
trackable_list_wrapper
Q
0
1
2
3
4
5
6"
trackable_list_wrapper
.
O0
P1"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
j
Q_initializer
t_create_resource
u_initialize
v_destroy_resourceR jCustom.StaticHashTable
O
w_create_resource
x_initialize
y_destroy_resourceR Z
tablez{
"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
N
	Rtotal
	Scount
T	variables
U	keras_api"
_tf_keras_metric
^
	Vtotal
	Wcount
X
_fn_kwargs
Y	variables
Z	keras_api"
_tf_keras_metric
"
_generic_user_object
:  (2total
:  (2count
.
R0
S1"
trackable_list_wrapper
-
T	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
V0
W1"
trackable_list_wrapper
-
Y	variables"
_generic_user_object
/:-
?N?2Adam/embedding_6/embeddings/m
':%	?2Adam/dense_13/kernel/m
 :2Adam/dense_13/bias/m
&:$2Adam/dense_12/kernel/m
 :2Adam/dense_12/bias/m
/:-
?N?2Adam/embedding_6/embeddings/v
':%	?2Adam/dense_13/kernel/v
 :2Adam/dense_13/bias/v
&:$2Adam/dense_12/kernel/v
 :2Adam/dense_12/bias/v
?2?
(__inference_model_6_layer_call_fn_240195
(__inference_model_6_layer_call_fn_240575
(__inference_model_6_layer_call_fn_240598
(__inference_model_6_layer_call_fn_240387?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
C__inference_model_6_layer_call_and_return_conditional_losses_240672
C__inference_model_6_layer_call_and_return_conditional_losses_240753
C__inference_model_6_layer_call_and_return_conditional_losses_240454
C__inference_model_6_layer_call_and_return_conditional_losses_240521?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?B?
!__inference__wrapped_model_240044input_7"?
???
FullArgSpec
args? 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
__inference_adapt_step_240801?
???
FullArgSpec
args?

jiterator
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
,__inference_embedding_6_layer_call_fn_240808?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
G__inference_embedding_6_layer_call_and_return_conditional_losses_240817?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
7__inference_global_max_pooling1d_6_layer_call_fn_240822
7__inference_global_max_pooling1d_6_layer_call_fn_240827?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240833
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240839?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
)__inference_dense_13_layer_call_fn_240848?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
D__inference_dense_13_layer_call_and_return_conditional_losses_240859?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
*__inference_dropout_6_layer_call_fn_240864
*__inference_dropout_6_layer_call_fn_240869?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
E__inference_dropout_6_layer_call_and_return_conditional_losses_240874
E__inference_dropout_6_layer_call_and_return_conditional_losses_240886?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
)__inference_dense_12_layer_call_fn_240895?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
D__inference_dense_12_layer_call_and_return_conditional_losses_240906?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
$__inference_signature_wrapper_240552input_7"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
__inference__creator_240911?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__initializer_240919?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__destroyer_240924?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__creator_240929?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__initializer_240934?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__destroyer_240939?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?B?
__inference_save_fn_240958checkpoint_key"?
???
FullArgSpec
args?
jcheckpoint_key
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *?	
? 
?B?
__inference_restore_fn_240966restored_tensors_0restored_tensors_1"?
???
FullArgSpec
args? 
varargsjrestored_tensors
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *?
	?
	?	
	J
Const
J	
Const_1
J	
Const_2
J	
Const_3
J	
Const_4
J	
Const_57
__inference__creator_240911?

? 
? "? 7
__inference__creator_240929?

? 
? "? 9
__inference__destroyer_240924?

? 
? "? 9
__inference__destroyer_240939?

? 
? "? B
__inference__initializer_2409193???

? 
? "? ;
__inference__initializer_240934?

? 
? "? ?
!__inference__wrapped_model_240044r	3|}~#$0?-
&?#
!?
input_7?????????
? "3?0
.
dense_12"?
dense_12?????????j
__inference_adapt_step_240801I4??<
5?2
0?-?
??????????IteratorSpec 
? "
 ?
D__inference_dense_12_layer_call_and_return_conditional_losses_240906\#$/?,
%?"
 ?
inputs?????????
? "%?"
?
0?????????
? |
)__inference_dense_12_layer_call_fn_240895O#$/?,
%?"
 ?
inputs?????????
? "???????????
D__inference_dense_13_layer_call_and_return_conditional_losses_240859]0?-
&?#
!?
inputs??????????
? "%?"
?
0?????????
? }
)__inference_dense_13_layer_call_fn_240848P0?-
&?#
!?
inputs??????????
? "???????????
E__inference_dropout_6_layer_call_and_return_conditional_losses_240874\3?0
)?&
 ?
inputs?????????
p 
? "%?"
?
0?????????
? ?
E__inference_dropout_6_layer_call_and_return_conditional_losses_240886\3?0
)?&
 ?
inputs?????????
p
? "%?"
?
0?????????
? }
*__inference_dropout_6_layer_call_fn_240864O3?0
)?&
 ?
inputs?????????
p 
? "??????????}
*__inference_dropout_6_layer_call_fn_240869O3?0
)?&
 ?
inputs?????????
p
? "???????????
G__inference_embedding_6_layer_call_and_return_conditional_losses_240817`/?,
%?"
 ?
inputs?????????d	
? "*?'
 ?
0?????????d?
? ?
,__inference_embedding_6_layer_call_fn_240808S/?,
%?"
 ?
inputs?????????d	
? "??????????d??
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240833wE?B
;?8
6?3
inputs'???????????????????????????
? ".?+
$?!
0??????????????????
? ?
R__inference_global_max_pooling1d_6_layer_call_and_return_conditional_losses_240839^4?1
*?'
%?"
inputs?????????d?
? "&?#
?
0??????????
? ?
7__inference_global_max_pooling1d_6_layer_call_fn_240822jE?B
;?8
6?3
inputs'???????????????????????????
? "!????????????????????
7__inference_global_max_pooling1d_6_layer_call_fn_240827Q4?1
*?'
%?"
inputs?????????d?
? "????????????
C__inference_model_6_layer_call_and_return_conditional_losses_240454l	3|}~#$8?5
.?+
!?
input_7?????????
p 

 
? "%?"
?
0?????????
? ?
C__inference_model_6_layer_call_and_return_conditional_losses_240521l	3|}~#$8?5
.?+
!?
input_7?????????
p

 
? "%?"
?
0?????????
? ?
C__inference_model_6_layer_call_and_return_conditional_losses_240672k	3|}~#$7?4
-?*
 ?
inputs?????????
p 

 
? "%?"
?
0?????????
? ?
C__inference_model_6_layer_call_and_return_conditional_losses_240753k	3|}~#$7?4
-?*
 ?
inputs?????????
p

 
? "%?"
?
0?????????
? ?
(__inference_model_6_layer_call_fn_240195_	3|}~#$8?5
.?+
!?
input_7?????????
p 

 
? "???????????
(__inference_model_6_layer_call_fn_240387_	3|}~#$8?5
.?+
!?
input_7?????????
p

 
? "???????????
(__inference_model_6_layer_call_fn_240575^	3|}~#$7?4
-?*
 ?
inputs?????????
p 

 
? "???????????
(__inference_model_6_layer_call_fn_240598^	3|}~#$7?4
-?*
 ?
inputs?????????
p

 
? "??????????z
__inference_restore_fn_240966Y4K?H
A?>
?
restored_tensors_0
?
restored_tensors_1	
? "? ?
__inference_save_fn_240958?4&?#
?
?
checkpoint_key 
? "???
`?]

name?
0/name 
#

slice_spec?
0/slice_spec 

tensor?
0/tensor
`?]

name?
1/name 
#

slice_spec?
1/slice_spec 

tensor?
1/tensor	?
$__inference_signature_wrapper_240552}	3|}~#$;?8
? 
1?.
,
input_7!?
input_7?????????"3?0
.
dense_12"?
dense_12?????????