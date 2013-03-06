(* ::Package:: *)

(* ::Title:: *)
(*stdlib.*)


(* ::Subtitle:: *)
(*Wiktor Macura*)


Stdlib::msg="`1`";
Error[msg___]:=Message[Stdlib::msg,Row[{msg}]]


(* ::Section:: *)
(*Expression Utilities*)


ContainsQ[expr_, patt_] := Not[FreeQ[expr, patt]]


(* ::Section:: *)
(*String Utilities*)


StringShort[s_, len_:50] :=
Module[{l,r},
	If[StringLength[s] < len,
		s,
		l = Ceiling[len/3];
		r = Floor[2 len/3];
		StringTake[s,l]<>"\[Ellipsis]"<>StringTake[s, -r]
	]
];


(* ::Input:: *)
(*StringShort["hello there",15]*)


(* ::Input:: *)
(*StringShort["hello there",10]*)


StringContainsQ[expr_, s_] := Not[StringFreeQ[expr, s]]


ToStandardName[s_String]:=
	StringReplace[
		StringReplace[s, WordBoundary~~ch_ :> ToUpperCase[ch]],
		{
			ch:CharacterRange["a","z"]|CharacterRange["0","9"]:>ch,
			_->""
		},
		IgnoreCase->True
	]


(* ::Input:: *)
(*ToStandardName["the `united states of america12312"]*)


Clear[StringDecompose];
StringDecompose[body_, separator_->pairs_, lhsfn_:ToStandardName, rhsfn_:StringTrim]:=
	lhsfn[#1]->rhsfn[#2]&@@@StringSplit[StringSplit[body,separator],pairs]


(* ::Input:: *)
(*StringDecompose["numberOfStores=1, numberOfStorefiles=1, storefileUncompressedSizeMB=209, storefileSizeMB=45, compressionRatio=0.2153, memstoreSizeMB=0, storefileIndexSizeMB=0, readRequestsCount=0, writeRequestsCount=0, rootIndexSizeKB=276, totalStaticIndexSizeKB=0, totalStaticBloomSizeKB=321, totalCompactingKVs=0, currentCompactedKVs=0, compactionProgressPct=NaN, coprocessors=[]",","->"="]*)


(* ::Section:: *)
(*Date Utilities*)


DateInterval[spec_]:=
Module[{lo=spec,hi=spec},
	hi = ReplacePart[hi,-1->hi[[-1]]+1];
	{DateList[lo],DateList[AbsoluteTime[hi]-1]}
]


(* ::Input:: *)
(*DateInterval[{2013}]*)


(* ::Input:: *)
(*DateInterval[{2013,1}]*)


(* ::Input:: *)
(*DateInterval[{2013,1,15}]*)


(* ::Input:: *)
(*DateInterval[{2013,1,15,12}]*)


With[{epochStart=AbsoluteTime[{1970,1,1,0,0,0}]},
FromUnixTime[ts_]:=
epochStart+ts
]


$dateIntervals={
	"Millisecond"->1,
	"Second"->1000,
	"Hour"->1000*3600,
	"Day"-> 1000*3600*24,
	"Week"->1000*3600*24*7,
	"Month"->1000*3600*24*30,
	"Year"->1000*3600*24*365
};


(* ::Input:: *)
(*AbsoluteTime["dec 1"]*)


FindDateDivisions[min_,max_,num_]:=
Union[Flatten[Quiet[Developer`FindDivisions[{min,max,Reverse@Most[$dateIntervals[[All,2]]]},{num,1,1,1,1,1},num]]]]


(* ::Input:: *)
(*DateString/@FindDateDivisions[AbsoluteTime["dec 1"],AbsoluteTime["dec 31"],10]*)


DateFormat[min_,max_,scalar_:1]:=
	Module[{width=scalar*(max-min)},
		Which[
			width<1000,{"Second",".","Milliseconds"},
			width<1000*3600,{"Hour12",":","Minute",":","Second",".","Millisecond"},
			width<1000*3600*24,{"Hour12",":","Minute",":","Second","AMPMLowerCase"},
			width<1000*3600*24*7,{"Month","/","Day"," ","Hour12",":","Minute","AMPMLowerCase"},
			width<1000*3600*24*30,{"Month","/","Day"," ","Hour12","AMPMLowerCase"},
			width<1000*3600*24*365,{"Month","/","Day","/","Year"},
			width<1000*3600*24*365*5,{"Month","/","Year"},
			True,{"Year"}
		]
	]


DateHistogram[data_,options___]:=
	Module[{max,min,del,format},
		max=Max[data];
		min=Min[data];
		del=max-min;
		format=DateFormat[min,max,10];
		Histogram[data,options,
			Ticks->{{#,DateString[#,format]}&/@FindDivisions[{min,max},10],Automatic}
		]
	]


(* ::Section::Closed:: *)
(*List Utilities*)


MapProgress[fn_, list_, o___] :=
 Module[{pos = 0, len = Length[list]},
  PrintTemporary[Dynamic[
    Row[{
      "Elem ", pos, " of ", len
      }, " "], UpdateInterval -> .2, TrackedSymbols -> {}]];
  MapIndexed[
   Function[{e, indx},
    pos = indx;
    fn[e]
    ],
   list,
   o
   ]
  ]


ParallelMapProgress[fn_,list_,o___]:=
Module[{pos=0,len=Length[list]},
PrintTemporary[Dynamic[
Row[{
"Elem ",pos," of ",len
}," "],UpdateInterval->.2,TrackedSymbols->{}]];
MapIndexed[
Function[{e,indx},
pos=indx;
fn[e]
],
list,
o
]
]


(* ::Input:: *)
(*f=MapProgress[Function[Pause[.1];#],Range[10]];*)
(*FullForm[f]*)


(* ::Section:: *)
(*Graphics*)


(* ::Subsection::Closed:: *)
(*Colors*)


colors = "$base03:#002b36;
  $base02:#073642;
  $base01:#586e75;
  $base00:#657b83;
  $base0:#839496;
  $base1:#93a1a1;
  $base2:#eee8d5;
  $base3:#fdf6e3;
  $yellow:#b58900;
  $orange:#cb4b16;
  $red:#dc322f;
  $magenta:#d33682;
  $violet:#6c71c4;
  $blue:#268bd2;
  $cyan:#2aa198;
  $green:#859900;";


colors01=StringSplit[
StringReplace[StringSplit[colors,"\n"],"$"|"#"|";"->""],
":"
];


HexColor[v_]:=
RGBColor@@(FromDigits[StringJoin[#],16]/256&/@Partition[Characters[v],2])


ToHexColor[k_]:=StringJoin[IntegerString[#,16]&/@Round[List@@k*256]]


(* ::Input:: *)
(*"2aa198"==ToHexColor[HexColor["2aa198"]]*)


Clear[Solarized];
Solarized[#1]=HexColor[#2];&@@@colors01;


Solarized[]=colors01[[All,1]];


ColorSquare[k_]:=Graphics[{EdgeForm[Gray],k,Rectangle[]},ImageSize->20]


ColorSquare[Solarized[#]]&/@Solarized[]


Solarized["MonochromeBlend"]=Function[Blend[Solarized/@Solarized[][[;;8]],#]];
Solarized["ColorBlend"]=Function[Blend[Solarized/@Solarized[][[9;;]],#]];


(* ::Input:: *)
(*ColorSquare[Solarized["MonochromeBlend"][#]]&/@Range[0,1,.05]//Row*)


(* ::Input:: *)
(*ColorSquare[Solarized["ColorBlend"][#]] & /@ Range[0, 1, .05] // Row*)


(* ::Subsection:: *)
(*Base Styles*)


DataLabel[ex_, opts___] := Style[ex,9,Solarized["base03"],FontFamily->"Tahoma",opts]


$PlotBaseStyle = Sequence[
	FrameStyle->Solarized["base03"],
	BaseStyle->{9,Solarized["base03"],FontFamily->"Tahoma"},
	ImageSize->Large
];


(* ::Input:: *)
(*Show[Plot[Sin[x],{x,-5,5}],$PlotBaseStyle]*)


Clear[DataGrid];
DataGrid[g_,labels:{___String}:{},opts___]:=
	Grid[
		If[labels=!={},
			Prepend[g, labels],
			g
		],
		Dividers->Solarized["base3"],
		BaseStyle->{Solarized["base03"],FontFamily->"Tahoma"},
		Alignment->{".",Baseline},
		Spacings->{2,.1},
		opts
	];


(* ::Input:: *)
(*DataGrid[Table["xxx",{13},{15}]]*)


(* ::Input:: *)
(*DataGrid[Table["xxx",{13},{15}],Table["c",{15}]]*)


DataLabeled[item_,label_,opts___]:=
	Column[
{Style[label,Solarized["base03"],FontFamily->"Tahoma"],item},
Dividers->{White,{White,Directive[Thick,Solarized["base2"]],White}}
]


(* ::Input:: *)
(*DataLabeled["xxasdfasdfasdfx","yyy"]*)


(* ::Section::Closed:: *)
(*Advanced Plotting*)


(* ::Subsection:: *)
(*Two Axis*)


ShowTwoAxis[fgraph_,ggraph_,opts___]:=
Module[{frange,grange,fticks,gticks},{frange,grange}=(PlotRange/.AbsoluteOptions[#,PlotRange])[[2]]&/@{fgraph,ggraph};fticks=N@FindDivisions[frange,5];gticks=Quiet@Transpose@{fticks,ToString[NumberForm[#,2],StandardForm]&/@Rescale[fticks,frange,grange]};Show[fgraph,ggraph/.Graphics[graph_,s___]:>Graphics[GeometricTransformation[graph,RescalingTransform[{{0,1},grange},{{0,1},frange}]],s],Axes->False,Frame->True,FrameTicks->{{fticks,gticks},{Automatic,Automatic}}]]


ShowTwoAxis[
Plot[3Sin[x],{x,-5,5}],
Plot[5Cos[x],{x,-5,5}]
]


Margined[f_]:=Framed[f,FrameStyle->White]


(* ::Section:: *)
(*Archive Import*)


Options[Archive]={
	"ArchiveTime" :> AbsoluteTime[],
	"ArchiveDirectory":>FileNameJoin[{HomeDirectory[],"MArchive"}],
	"ArchiveVersion"->Automatic,
	"Timeout"->15 (* in minutes *)
};


Needs["JLink`"];
InstallJava[];


URLParts[url_] :=
Module[{uO = JavaNew["java.net.URL", url], rls},
	rls = {
		"Host" ->  uO@getHost[],
		"Port" ->  uO@getPort[],
		"Path" ->  uO@getPath[],
		"Query" -> uO@getQuery[],
		"Protocol" -> uO@getProtocol[]
	} /. Null|-1 -> None;
	ReleaseJavaObject[uO];
	DeleteCases[rls, _->None]
]


hashUrl[source_] := IntegerString[Hash[source],36]


deconstructUrl[source_] :=
Module[{rls=URLParts[source], host,path,query},
	host = StringSplit["Host"/.rls, "."];
	path = StringSplit["Path"/.rls,"/"];

	query = "Query"/.rls;
	If[query != "Query",
		query = StringReplace[query, {c:CharacterRange["a","z"]|"="|"-":>c,"&"->",", w_:>URLEncoder`encode[w]}, IgnoreCase->True]
		,
		query = Sequence[]
	];
	Flatten[{
		StringJoin[Riffle[host[[-2;;]],"."]],
		#<>"."&/@Reverse[host[[;;-3]]],
		path,
		query,
		hashUrl[source]<>"-"<>DateString[{"Year","Month","Day","-","Hour","Minute","Second"}]
	}]
]


ArchivePath[source_, OptionsPattern[Archive]] :=
	Flatten[{
		OptionValue["ArchiveDirectory"], 
		deconstructUrl[source]
	}]


(* ::Input:: *)
(*ArchivePath["http://www.google.com"]*)


ArchiveQ[source_, opts:OptionsPattern[Archive]] :=
 Head[ArchiveFile[source, opts]]===String


ArchiveFile[source_, opts:OptionsPattern[Archive]]:=
Module[{files, sortedFiles, path=ArchivePath[source,opts],now=OptionValue["ArchiveTime"]},
	If[!FileExistsQ[FileNameJoin[Most[path]]],
		Return[None]
	];
	
	sortedFiles = SortBy[# -> (Date /. FileInformation[#])& /@ FileNames[FileNameJoin[Append[Most[path], "*"]]],Last];
	If[OptionValue["ArchiveVersion"] === Automatic,
		files = Select[
			sortedFiles,
			AbsoluteTime[DateList@Last[#]]> now-OptionValue["Timeout"]*60&,
			1
		];
		files[[-1,1]]
		,
		sortedFiles[[OptionValue["ArchiveVersion"],1]]
	]
]


FileInformation@ArchiveFile["http://www.google.com"]
ArchiveQ["http://www.google.com"]
ArchiveSave["http://www.google.com"]
ArchiveFile["http://www.google.com","ArchiveVersion"->{2013,3,6,}]


ArchiveData[source_, opts:OptionsPattern[Archive]] :=
Module[{path=ArchivePath[source, opts],files},
	If[!FileExistsQ[FileNameJoin[Most[path]]],
		Return[None];
	];
	
	files=FileNames[FileNameJoin[Append[Most[path], "*"]]];
	({File, Date, ByteCount} /.FileInformation[#])& /@ files
]

ArchiveInfo[source_, opts:OptionsPattern[Archive]]:=
	DataGrid[
		{StringShort[#1,50],DateString[#2],#3}& @@@ ArchiveData[source,opts],
		{"file","date","size"}
	]


ArchiveInfo["http://www.google.com"]


ArchiveSave[source_, opts : OptionsPattern[Archive]] :=
Module[{file=ArchiveFile[source, opts]},
 If[file =!= None,
  	file,
  	ArchiveForceSave[source, opts]
  ]
]


ArchiveForceSave[source_,opts:OptionsPattern[Archive]]:=
Module[{path=ArchivePath[source,opts]},
	Quiet[CreateDirectory[FileNameJoin[Most[path]]], CreateDirectory::filex];
	URLSave[source, FileNameJoin[path]];
	path
]


Clear[ArchiveImport];
ArchiveImport[source_,type_,opts:OptionsPattern[Archive]]:=
Module[{localfile},
	localfile=ArchiveSave[source,opts];
	Import[FileNameJoin[localfile],type]
]
