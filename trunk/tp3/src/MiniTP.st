<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of March 3, 2008 on November 14, 2008 at 9:40:31 am</time-stamp>
<!-- Package MiniTP* -->


<class>
<name>Pokemon</name>
<environment>Smalltalk</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>energia experiencia fuerza </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>MiniTP</package>
</attributes>
</class>

<class>
<name>Deoxys</name>
<environment>Smalltalk</environment>
<super>Pokemon</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>poderCampoDeFuerza </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>MiniTP</package>
</attributes>
</class>

<class>
<name>Pikachu</name>
<environment>Smalltalk</environment>
<super>Pokemon</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>MiniTP</package>
</attributes>
</class>

<class>
<name>Bulbasaur</name>
<environment>Smalltalk</environment>
<super>Pokemon</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>MiniTP</package>
</attributes>
</class>

<class>
<name>Raichu</name>
<environment>Smalltalk</environment>
<super>Pikachu</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>voltaje </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category></category>
<attributes>
<package>MiniTP</package>
</attributes>
</class>






<methods>
<class-id>Pokemon class</class-id> <category>instance creation</category>

<body package="MiniTP" selector="new:experiencia:fuerza:">new:energia experiencia:exp fuerza:f
	"Answer a newly created and initialized instance."
	^super new initialize:energia experiencia:exp fuerza:f</body>
</methods>


<methods>
<class-id>Pokemon</class-id> <category>initialize-release</category>

<body package="MiniTP" selector="initialize:experiencia:fuerza:">initialize:en experiencia:exp fuerza:f
	"Initialize a newly created instance. This method must answer the receiver."

	" *** Edit the following to properly initialize instance variables ***"
	energia := en.
	experiencia := exp.
	fuerza := f.
	" *** And replace this comment with additional initialization code *** "
	^self</body>
</methods>

<methods>
<class-id>Pokemon</class-id> <category>accessing</category>

<body package="MiniTP" selector="experiencia">experiencia
	^experiencia</body>

<body package="MiniTP" selector="energia:">energia: anObject
	energia := anObject</body>

<body package="MiniTP" selector="energia">energia
	^energia</body>

<body package="MiniTP" selector="experiencia:">experiencia: anObject
	experiencia := anObject</body>

<body package="MiniTP" selector="fuerza:">fuerza: anObject
	fuerza := anObject</body>

<body package="MiniTP" selector="fuerza">fuerza
	^fuerza</body>
</methods>

<methods>
<class-id>Pokemon</class-id> <category>private</category>

<body package="MiniTP" selector="debilitarse:">debilitarse:dano
	energia := energia - dano.</body>

<body package="MiniTP" selector="golpearA:">
golpearA:p
	p debilitarse:(fuerza*experiencia).</body>

<body package="MiniTP" selector="vive">vive
	^(energia &gt; 0)</body>

<body package="MiniTP" selector="pelearAMuerteContra:">pelearAMuerteContra:p
	[self vive &amp; p vive] whileTrue:[self golpearA:p. p golpearA:self].</body>
</methods>


<methods>
<class-id>Bulbasaur class</class-id> <category>instance creation</category>

<body package="MiniTP" selector="new:experiencia:">new:en experiencia:exp
	^super new initialize:en experiencia:exp</body>
</methods>


<methods>
<class-id>Bulbasaur</class-id> <category>private</category>

<body package="MiniTP" selector="debilitarse:">debilitarse:dano
	super debilitarse:dano.
	fuerza := dano + fuerza.</body>
</methods>

<methods>
<class-id>Bulbasaur</class-id> <category>initialize-release</category>

<body package="MiniTP" selector="initialize:experiencia:">initialize:en experiencia:exp
	"Initialize a newly created instance. This method must answer the receiver."
	super initialize:en experiencia:exp fuerza:0.
	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>
</methods>


<methods>
<class-id>Pikachu class</class-id> <category>instance creation</category>

<body package="MiniTP" selector="new:experiencia:">new:en experiencia:exp
	^super new initialize:en experiencia:exp</body>
</methods>


<methods>
<class-id>Pikachu</class-id> <category>initialize-release</category>

<body package="MiniTP" selector="initialize:experiencia:">initialize:en experiencia:exp
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize:en experiencia:exp fuerza:5.
	" *** Replace this comment with the appropriate initialization code *** "
	^self</body>
</methods>


<methods>
<class-id>Raichu class</class-id> <category>instance creation</category>

<body package="MiniTP" selector="new:experiencia:fuerza:voltaje:">new:en experiencia:exp fuerza:f voltaje:v
	^super new initialize:en experiencia:exp fuerza:f voltaje:v</body>
</methods>


<methods>
<class-id>Raichu</class-id> <category>initialize-release</category>

<body package="MiniTP" selector="initialize:experiencia:fuerza:voltaje:">initialize:en experiencia:exp fuerza:f voltaje:v
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize:en experiencia:exp fuerza:f.
	" *** Edit the following to properly initialize instance variables ***"
	voltaje := v.
	" *** And replace this comment with additional initialization code *** "
	^self</body>
</methods>

<methods>
<class-id>Raichu</class-id> <category>accessing</category>

<body package="MiniTP" selector="voltaje">voltaje
	^voltaje</body>

<body package="MiniTP" selector="voltaje:">voltaje: anObject
	voltaje := anObject</body>
</methods>

<methods>
<class-id>Raichu</class-id> <category>private</category>

<body package="MiniTP" selector="golpearA:">golpearA:p
	p debilitarse:(fuerza*experiencia+voltaje).</body>
</methods>


<methods>
<class-id>Deoxys class</class-id> <category>instance creation</category>

<body package="MiniTP" selector="new:experiencia:poderCampoDeFuerza:">new:en experiencia:exp poderCampoDeFuerza:cp
	^super new initialize:en experiencia:exp poderCampoDeFuerza:cp</body>
</methods>


<methods>
<class-id>Deoxys</class-id> <category>initialize-release</category>

<body package="MiniTP" selector="initialize:experiencia:poderCampoDeFuerza:">initialize:en experiencia:exp poderCampoDeFuerza:pc
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize:en experiencia:exp fuerza:1.
	" *** Edit the following to properly initialize instance variables ***"
	poderCampoDeFuerza := pc.
	" *** And replace this comment with additional initialization code *** "
	^self</body>
</methods>

<methods>
<class-id>Deoxys</class-id> <category>accessing</category>

<body package="MiniTP" selector="poderCampoDeFuerza">poderCampoDeFuerza
	^poderCampoDeFuerza</body>

<body package="MiniTP" selector="poderCampoDeFuerza:">poderCampoDeFuerza: anObject
	poderCampoDeFuerza := anObject</body>
</methods>

<methods>
<class-id>Deoxys</class-id> <category>private</category>

<body package="MiniTP" selector="debilitarse:">debilitarse:dano
	super debilitarse:(dano-poderCampoDeFuerza).</body>
</methods>



</st-source>
