--# -path=.:../abstract
concrete MiniGrammarEng of MiniGrammar = open MiniResEng, Prelude in {


  lincat
    Utt = {s : Str} ;
    --TODO Pol
    Pol  = {s : Str ; isTrue : Bool} ; -- the s field is empty, but needed for parsing
    --TODO Temp
    Temp = {s : Str ; isPres : Bool} ;
    
    S  = {s : Str} ;
    --TODO QS
    QS = {s : Str} ;
    --TODO Cl
    Cl = {   -- word order is fixed in S and QS
      subj : Str ;                             -- subject
      verb : Bool => Bool => {fin,inf : Str} ; -- dep. on Pol,Temp, e.g. "does","sleep"
      compl : Str                              -- after verb: complement, adverbs
      } ;
    --TODO QC:
    QCl = Cl ** {isWh : Bool} ;
    --TODO Imp
    Imp = {s : Bool => Str} ;
    --TODO what is GVerb??
    VP = {verb : GVerb ; compl : Str} ;
    Comp = {s : Gender => Number => Str} ; --needed to fit complements' gender and number to that of the subject, see above
    AP = Adjective ; --{s : Gender => Number => Case => Str}
    CN = Noun ; --{s : Number => Case => Str ; g : Gender}
    NP =  {s : Case => Str ; a: NPAgreement ; g : Gender ; n : Number ; isPron : Bool } ; --NPAgreement and isPron for when it is subject, g and n for when it is an object.
    --TODO IP
    IP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a: NPAgreement ; g : Gender ; n : Number ; isPron : Bool } ;
    Det = {s : Gender => Case => Str ; n : Number} ;
    --TODO Conj
    Conj = {s : Str} ;
    Prep = {s : Str ; c : Case} ;
    V = Verb ;
    V2 = Verb2 ;
    --TODO VS
    VS = Verb ;
    --TODO VV
    VV = Verb ; ---- only VV to VP
    A = Adjective ;
    N = Noun ;
    --TODO PN
    PN = Noun ;
    Adv = {s : Str} ;
    --TODO IAdv
    IAdv = {s : Str} ;

  lin
  -- Phrase
    UttS s = s ;
    --TODO UttQS
    UttQS s = s ;
    --originally it was Acc here, but, to be honest, a NP in any case could be an 
    --utterance here, so I picked the default Nom. In addition, to questions that
    --would normally be answered with "me" in English (e.g. "who did it?") we would
    --normally answer in nominative in Polish.
    UttNP np = {s = np.s ! Nom} ; 
    UttAdv adv = adv ;
    UttIAdv iadv = iadv ;
    --TODO UttImpSg
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.isTrue} ;

  -- Sentence and clause
    --TODO UseCl
    UseCl temp pol cl =
      let clt = cl.verb ! pol.isTrue ! temp.isPres  -- isTrue regulates if "do" is used
      in {
        s = pol.s ++ temp.s ++    --- needed for parsing: a GF hack
	    cl.subj ++               -- she
	    clt.fin ++               -- does
	    negation pol.isTrue ++   -- not
	    clt.inf ++               -- drink
	    cl.compl                 -- beer
      } ;

    --TODO UseQCl 
    UseQCl temp pol qcl =
      let
         isWh = qcl.isWh ;
         clt = qcl.verb ! andB isWh pol.isTrue ! temp.isPres ;  -- no "do" in present positive Wh questions
         verbsubj = case isWh of {
	    True  => qcl.subj ++ clt.fin ;      -- no inversion in Wh questions
	    False => clt.fin ++ qcl.subj
	    }

      in {
        s = pol.s ++ temp.s ++
	    verbsubj ++
	    negation pol.isTrue ++   -- not
	    clt.inf ++               -- drink
	    qcl.compl                -- beer
      } ;

    --TODO PredVP
    PredVP np vp = {
      subj = np.s ! Nom ;
      compl = vp.compl ;
      verb = \\plain,isPres => case <vp.verb.isAux, plain, isPres, np.a> of {

        -- non-auxiliary verbs, negative/question present: "does (not) drink" 
        <False,False,True,Agr Sg Per3> => {fin = "does" ; inf = vp.verb.s ! VF Inf} ;
        <False,False,True,_          > => {fin = "do"   ; inf = vp.verb.s ! VF Inf} ;
	
        -- non-auxiliary, plain present ; auxiliary, all present: "drinks", "is (not)"
        <_,_, True, Agr Sg Per1> => {fin = vp.verb.s ! PresSg1    ; inf = []} ;
        <_,_, True, Agr Sg Per3> => {fin = vp.verb.s ! VF PresSg3 ; inf = []} ;
        <_,_, True, _>           => {fin = vp.verb.s ! PresPl     ; inf = []} ;

        -- all verbs, past: "has (not) drunk", "has (not) been"
        <_,_, False,Agr Sg Per3> => {fin = "has"  ; inf = vp.verb.s ! VF PastPart} ;
        <_,_, False,_          > => {fin = "have" ; inf = vp.verb.s ! VF PastPart} 

        -- the negation word "not" is put in place in UseCl, UseQCl
      }
    } ;

    --TODO QuestCl
    QuestCl cl = cl ** {isWh = False} ; -- since the parts are the same, we don't need to change anything
    
    --TODO QuestVP
    QuestVP ip vp = PredVP ip vp ** {isWh = True} ; 

    --TODO ImpVP
    ImpVP vp = {
      s = table {
        True  => vp.verb.s ! VF Inf ++ vp.compl ;    -- in Eng, imperative = infinitive
        False => "do not" ++ vp.verb.s ! VF Inf ++ vp.compl
        }
      } ;

  -- Verb
    --do I need generalized verbs? no
    UseV v = {
      verb = v ;
      compl = table {g => table {n => []}} ; --no object or complement
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = table {g => table {n => v2.cp ++ np.s ! Acc}}  -- NP object in the accusative, potential preposition first
      } ;

    --TODO ComplVS  
    ComplVS vs s = {
      verb = verb2gverb vs ;
      compl = "that" ++ s.s ;
      } ;

    --TODO ComplVV  
    ComplVV vv vp = {
      verb = verb2gverb vv ;
      compl = "to" ++ vp.verb.s ! VF Inf ++ vp.compl ;
      } ;
      
    UseComp comp = {
      verb = be_Verb ; -- the verb is the copula "be"
      compl = comp.s --subject complement, can later select the appropriate form based on gender and number of the subject
      } ;
      
    CompAP ap = {
      s = table {
        g => table {
          n => ap.s ! g ! n ! Nom --allows to select the appropriate form based on gender and number of the subject
        }
      }
    } ;
      
    --TODO CompNP
    CompNP np = {
      s = np.s ! Nom    -- NP complement is in the nominative
      } ;

    --TODO CompAdv 
    CompAdv adv = adv ;
 
    --for this one it is worth noting that adverbs are very flexible in terms of position and this sentence-final position works well for
    --adverbials like "in a book" or "with them", but not as much for adverbs like "now" or "already", it sounds a bit off, however, I believe
    --that fixing this would be rather complicated and the solution I can think of does not allow for multiple adverbs.
    AdvVP vp adv = {
      verb = vp.verb ;
      compl = table {g => table {n => vp.compl ! g ! n ++ adv.s}} --adds an adv to the object or complement
    } ;

  --Noun  
    DetCN det cn = {
      s = table {c => det.s ! cn.g ! c ++ cn.s ! det.n ! c} ;
      a = NPAgr det.n Third ;
      g = cn.g ;
      n = det.n ;
      isPron = False --lets decide if this can be dropped when it is a subject
      } ;

    --TODO UsePN 
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
      
    UsePron p = p ;  -- Pron is worst-case NP  

    --TODO MassNP  
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = Agr Sg Per3
      } ;
      
    --it's worth pointing out that the definite "articles" here are somewhere between that and determiners ("this"). I decided to keep them 
    --nonetheless because they do highlight the difference between "some noun" and "the noun". The choice to keep them was motivated by some
    --Polish linguists arguing that it is an article in development and it really does act like it sometimes, for distinguishing definiteness.
    a_Det = {s = table {g => table {c => ""}} ; n = Sg} ;
    aPl_Det = {s = table {g => table {c => ""}} ; n = Pl} ; 
    the_Det = {s = table {
      MascAnim => table {
                Nom => "ten" ;
                Gen => "tego" ;
                Dat => "temu" ;
                Acc => "tego" ;
                Ins => "tym" ;
                Loc => "tym" ;
                Voc => "ten"
      } ;
      Masc => table {
                Nom => "ten" ;
                Gen => "tego" ;
                Dat => "temu" ;
                Acc => "ten" ;
                Ins => "tym" ;
                Loc => "tym" ;
                Voc => "ten"
      } ;
      Fem => table {
                Nom => "ta" ;
                Gen => "tej" ;
                Dat => "tej" ;
                Acc => "tę" ;
                Ins => "tą" ;
                Loc => "tej" ;
                Voc => "ta"
      } ;
      Neut => table {
                Nom => "to" ;
                Gen => "tego" ;
                Dat => "temu" ;
                Acc => "to" ;
                Ins => "tym" ;
                Loc => "tym" ;
                Voc => "to"
      }
    } ; n = Sg } ;

    thePl_Det = {s = table {
      --here only the MascAnim one is different
      MascAnim => table {
                Nom => "ci" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "tych" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "ci"
      } ;
      Masc => table {
                Nom => "te" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "te" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "te"
      } ;
      Fem => table {
                Nom => "te" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "te" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "te"
      } ;
      Neut => table {
                Nom => "te" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "te" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "te"
      }
    } ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => table {
                c => ap.s ! cn.g ! n ! c ++ cn.s ! n ! c --all number and case combinations with appropriate gender
       }} ; g = cn.g
      } ;

  -- Adjective
    PositA a = a ;

  -- Adverb
    PrepNP prep np = {s = prep.s ++ np.s ! prep.c} ; --the attached NP must be in an appropriate case

--TODO ALL BELOW
  -- Conjunction
    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

  -- Polarity  
    PPos  = {s = [] ; isTrue = True} ;
    PNeg  = {s = [] ; isTrue = False} ;

  -- Temporal
    TSim  = {s = []    ; isPres = True} ;
    TAnt  = {s = []    ; isPres = False} ;

    and_Conj = {s = "and"} ;
    or_Conj = {s = "or"} ;

  -- Structural
    every_Det = {s = table {
      --here MascAnim and Masc are identical
      MascAnim => table {
                Nom => "każdy" ;
                Gen => "każdego" ;
                Dat => "każdemu" ;
                Acc => "każdego" ;
                Ins => "każdym" ;
                Loc => "każdym" ;
                Voc => "każdy"
      } ;
      Masc => table {
                Nom => "każdy" ;
                Gen => "każdego" ;
                Dat => "każdemu" ;
                Acc => "każdego" ;
                Ins => "każdym" ;
                Loc => "każdym" ;
                Voc => "każdy"
      } ;
      Fem => table {
                Nom => "każda" ;
                Gen => "każdej" ;
                Dat => "każdej" ;
                Acc => "każdą" ;
                Ins => "każdą" ;
                Loc => "każdej" ;
                Voc => "każda"
      } ;
      Neut => table {
                Nom => "każde" ;
                Gen => "każdego" ;
                Dat => "każdemu" ;
                Acc => "każde" ;
                Ins => "każdym" ;
                Loc => "każdym" ;
                Voc => "każde"
      }
    } ; n = Sg} ;

    --Variants depending on the first consonant of the noun, stored case that is required of the NP that connects to the preposition
    --the "ze" and "we" variants occur after certain consonant clusters (f or w + consonant, s, z, ś, ź, ż, rz, sz + consonant, respectively),
    --but pre does not allow for gluing tokens together, so I was not able to do ("f"|"w") + (consonants). Instead I went over what clusters
    --are possible, with the help of https://sjp.pwn.pl/so/lista/A.html, and ended up with these voluminous lists. There is perhaps an easier
    --way of doing it, but I could not think of any at that point, knowing that pre takes precedence over gluing.
    --In addition, he cases assigned to these prepositions do not account for all their meanings; "z" can also mean from, as in "I am from Poland";
    --in that case it takes the Genitive case. However, I assume that would not be grouped together here, but would just be under from_Prep, 
    --together with the same set of "exceptions", but a different case.

    in_Prep = {s = pre {("fia"|"fią"|"fie"|"fię"|"fio"|"fiu"|"fl"|"fr"|"ft"|
                        "wb"|"wc"|"wd"|"wg"|"wia"|"wią"|"wie"|"wię"|"wio"|"wiu"|
                        "wj"|"wk"|"wl"|"wł"|"wm"|"wn"|"wp"|"wr"|"ws"|"wt"|"wz"|"wż") => "we" ; 
                        _ => "w"} ; c = Loc} ; --locative
    on_Prep = {s = "na" ; c = Loc} ; --locative
    with_Prep = {s = pre {("sc"|"sf"|"sg"|"sj"|"sk"|"sl"|"sł"|"sm"|"sn"|"sp"|"sr"|"st"|"sw"|
                          "zb"|"zd"|"zg"|"zj"|"zl"|"zł"|"zm"|"zn"|"zr"|"zs"|"zw"|"zż"|
                          "śc"|"śl"|"śm"|"śn"|"śp"|"śr"|"św"|
                          "źd"|"źl"|"źr"|
                          "żb"|"żd"|"żg"|"żl"|"żł"|"żm"|"żn"|"żr"|"żw"|
                          "rzg"|"rzn"|"rzp"|
                          "szc"|"szk"|"szl"|"szł"|"szm"|"szn"|"szp"|"szr"|"szt"|"szw") => "ze" ; 
                          _ => "z"} ; c = Ins} ; --instrumental

    --For the majority of pronouns there are alternative versions (e.g. for masculine accusative singular
    --there is "jego", "go", "niego", "-ń"); these have a relatively predictable distribution, but in some
    --contexts they are interchangeable. I think implementing all of them is beyond the scope of this assignment.
    --The "n" forms appear after prepositions. Short forms (without "je", wherever applicable) can only appear
    --in unstressed positions (which means what in the SVO sentences they are likely to appear in that form).
    --"-ń" is a clitic of some prepositions, and only works for the masculine.
    
    i_Pron = {
      s = table {
                Nom => "on" ;
                Gen => "mnie" ;
                Dat => "mi" ; 
                Acc => "mnie" ; 
                Ins => "mną" ;
                Loc => "mnie" ;
                Voc => "ja"
                } ;
      a = NPAgr Sg First ;
      --Here occurs a similar issue as for "they", though the form of the pronoun
      --stays the same, but it can occur in all the different genders when it comes
      --to past verb forms or subject complements. Because as the author of this 
      --grammar I go by feminine forms, this is the one I decided to give to this 
      --pronoun.
      g = Fem ;
      n = Sg ;
      isPron = True ;
      } ;

    youSg_Pron = {
      s = table {
                Nom => "ty" ;
                Gen => "cię" ; --or "ciebie"
                Dat => "tobie" ; 
                Acc => "cię" ; --or "ciebie"
                Ins => "tobą" ;
                Loc => "tobie" ;
                Voc => "ty"
                } ;
      a = NPAgr Sg Second ;
      --Same as above, this can be of any gender. For variety I chose the Masculine here.
      g = MascAnim ; 
      n = Sg ;
      isPron = True ;
      } ;

    he_Pron = {
      s = table {
                Nom => "on" ;
                Gen => "jego" ;
                Dat => "mu" ; --"jemu" is also acceptable and even preferred in certain contexts (when fronted for emphasis).
                Acc => "go" ; --"jego" is treated the same way as above.
                Ins => "nim" ;
                Loc => "nim" ;
                Voc => "on"
                } ;
      a = NPAgr Sg Third ;
      g = MascAnim ; 
      n = Sg ;
      isPron = True ;
      } ;
    she_Pron = {
      s = table {
                Nom => "ona" ;
                Gen => "jej" ;
                Dat => "jej" ; 
                Acc => "ją" ; 
                Ins => "nią" ;
                Loc => "niej" ;
                Voc => "ona"
                } ;
      a = NPAgr Sg Third ;
      g = Fem ;
      n = Sg ;
      isPron = True ;
      } ;
    we_Pron = {
      s = table {
                Nom => "my" ;
                Gen => "nas" ;
                Dat => "nam" ; 
                Acc => "nas" ; 
                Ins => "nami" ;
                Loc => "nas" ;
                Voc => "my"
                } ;
      a = NPAgr Pl First ;
      --Again, this works for both genders. Here I pick the feminine.
      g = Fem ;
      n = Pl ;
      isPron = True ;
      } ;

    youPl_Pron = {
      s = table {
                Nom => "wy" ;
                Gen => "was" ;
                Dat => "wam" ; 
                Acc => "was" ; 
                Ins => "wami" ;
                Loc => "was" ;
                Voc => "wy"
                } ;
      a = NPAgr Pl Second ;
      --Once more, the same issue with gender; and again I go with MascAnim.
      g = MascAnim ;
      n = Pl ;
      isPron = True ;
      } ;

    they_Pron = {
      s = table {
                Nom => "oni" ;
                Gen => "ich" ;
                Dat => "im" ; 
                Acc => "ich" ;
                Ins => "nimi" ;
                Loc => "nich" ;
                Voc => "oni"
                } ;
      a = NPAgr Pl Third ;
      g = MascAnim ;
      n = Pl ;
      isPron = True ;
      } ;
      --this one is used for plurals where no masculine human noun is a part of the group denoted by the pronoun; since this distinction seems
      --to be outside the scope of this assignment, I just commented this option out. This is only really relevant in actual discourse when we
      --know what the pronoun refers to, and in sentences with past verb forms, where they have to align in gender. Adjectives also have to do
      --that, but that is more straightforward.
      --they_nonmasc_Pron = {
      --s = table {
                --Nom => "one" ;
                --Gen => "ich" ;
                --Dat => "im" ; 
                --Acc => "je" ;
                --Ins => "nimi" ;
                --Loc => "nich" ;
                --Voc => "one"
                --} ;
      --a = NPAgr Pl Third ;
      --g = Fem ; -- or Neut or Masc
      --n = Pl ;
      --isPron = True ;
      --} ;

    whoSg_IP = {
      s = table {
                Nom => "kto" ;
                Gen => "kogo" ;
                Dat => "komu" ; 
                Acc => "kogo" ; 
                Ins => "kim" ;
                Loc => "kim" ;
                Voc => variants {} --make sure this works
                } ;
      a = NPAgr Sg Third ;
      --For once the choice of gender here is straightforward, as this pronoun only takes masculine forms, even if
      --intended to be used to talk about a woman; if we want to use feminine forms, we have to ask "which person"
      --instead, since "person" is a feminine noun.
      g = MascAnim ;
      n = Sg ;
      isPron = True ;
      } ;

    where_IAdv = {s = "gdzie"} ;
    why_IAdv = {s = "czemu"} ; --also "po co" or "dlaczego", maybe use variants?

    --TODO have_V2
    have_V2 = mkVerb "have" "has" "had" "had" "having" ** {c = []} ;

    --TODO want_VV
    want_VV = regVerb "want" ;
    
}
