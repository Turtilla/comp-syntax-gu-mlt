--# -path=.:../abstract
concrete MiniGrammarPol of MiniGrammar = open MiniResPol, Prelude in {


  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; isTrue : Bool} ; 
    Temp = {s : Str ; isPres : Bool} ;
    
    S  = {s : Str} ;
    QS = {s : Str} ;
    Cl = {
      subj : Str ;                                -- the subject 
      g : Gender ;                                -- the gender of the subject to determine complement
      n : Number ;                                -- the number of the subject to determine complement
      isPron : Bool ;                             -- determines if the subject is a pronoun
      verb : Bool => Str ;                        -- dep. on Temp, e.g. "robi","śpi"
      compl : Gender => Number => Case => Str ;   -- after verb: complement, adverbs
      complIsPron : Bool ;                        -- determines if the object is a pronoun
      rp : Case ;                                 -- object case in positive sentences
      rn : Case                                   -- object case in negative sentences
      } ;
    QCl = Cl ** {isWh : Bool} ;
    Imp = {s : Bool => Str} ; --only because as far as I understood, we are only doing 2nd person singular imperative, 1st and 2nd person plural would complicate things.
    VP = {verb : Verb ; compl : Gender => Number => Case => Str ; complIsPron : Bool ; rp : Case ; rn : Case} ;
    Comp = {s : Gender => Number => Str ; complIsPron : Bool} ; --needed to fit complements' gender and number to that of the subject, see above
    AP = Adjective ; --{s : Gender => Number => Case => Str}
    CN = Noun ; --{s : Number => Case => Str ; g : Gender}
    NP =  {s : Case => Str ; a: NPAgreement ; a2: NPGAgreement ; g : Gender ; n : Number ; isPron : Bool } ;  --NPAgreement and isPron for when it is subject in a present tense sentence,
    IP = {s : Case => Str ; a : NPAgreement ; a2: NPGAgreement ; g : Gender ; n : Number ; isPron : Bool } ;  --g and n for when it is an object, NPG when it is a subject in a past
    Pron = {s : Case => Str ; a: NPAgreement ; a2: NPGAgreement ; g : Gender ; n : Number ; isPron : Bool } ; --tense sentence.
    Det = {s : Gender => Case => Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = {s : Str ; c : Case} ; --prepositions require their NPs to take specific cases
    V = Verb ;
    V2 = Verb2 ;
    VS = Verb ;
    VV = Verb ;
    A = Adjective ;
    N = Noun ;
    PN = Noun ;
    Adv = {s : Str} ;
    IAdv = {s : Str} ;

  lin
  -- Phrase
    UttS s = s ;
    UttQS s = s ;
    --originally it was Acc here, but, to be honest, a NP in any case could be an utterance here, so I picked the default Nom. 
    --In addition, to questions that would normally be answered with "me" in English (e.g. "who did it?") we would normally answer in 
    --nominative in Polish ("kto to zrobił?" "ja (to zrobiłem)" / "(to byłem) ja").
    UttNP np = {s = np.s ! Nom} ; 
    UttAdv adv = adv ;
    UttIAdv iadv = iadv ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.isTrue} ;

  -- Sentence and clause
    UseCl temp pol cl = {
      s = case <cl.isPron, cl.complIsPron, pol.isTrue> of {
        --word order (or presence) depends on which elements of the sentence are pronouns. Polarity introduces negation, and tense is
        --inconsequential, as we do not have a generally used compound past tense.
        <True,_,True> => cl.verb ! temp.isPres ++ cl.compl ! cl.g ! cl.n ! cl.rp ;                          --"zrobiła to", "zrobiła auto"
        <True,_,False> => "nie" ++ cl.verb ! temp.isPres ++ cl.compl ! cl.g ! cl.n ! cl.rn  ;               --"nie zrobiła tego", "nie zrobiła auta"
        <False,True,True> => cl.subj ++ cl.compl ! cl.g ! cl.n ! cl.rp ++ cl.verb ! temp.isPres ;           --"kotka to zrobiła"
        <False,True,False> => cl.subj ++ cl.compl ! cl.g ! cl.n ! cl.rn ++ "nie" ++ cl.verb ! temp.isPres ; --"kotka tego nie zrobiła"
        <False,False,True> => cl.subj ++ cl.verb ! temp.isPres ++ cl.compl ! cl.g ! cl.n ! cl.rp ;          --"kotka zrobiła auto"
        <False,False,False> => cl.subj ++ "nie" ++ cl.verb ! temp.isPres ++ cl.compl ! cl.g ! cl.n ! cl.rn  --"kotka nie zrobiła auta"
      }
    } ;

    UseQCl temp pol qcl =
    {
      s = case <qcl.isWh, qcl.isPron, pol.isTrue, qcl.complIsPron> of {
        --similarly to above, but here it also matters if it is a wh- question. Some word orders are the same, but impossible to group up
        --using these conditions. I believe there is one set of conditions that, according to the compiler, is never reached here, but
        --technically it is possible to have a sentence like that so I am keeping it in. 
        <True,_True,True> => qcl.subj ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rp ++ qcl.verb ! temp.isPres ;                 --"kto to zrobił"
        <True,_,False,True> => qcl.subj ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rn ++ "nie" ++ qcl.verb ! temp.isPres ;      --"kto tego nie zrobił"

        <False,False,True,True> => "czy" ++ qcl.subj ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rp ++ qcl.verb ! temp.isPres ;  --"kot to zrobił"
        <False,False,False,True> => qcl.subj ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rn ++ "nie" ++ qcl.verb ! temp.isPres ; --"kot tego nie zrobił"

        <False,True,True,_> => qcl.verb ! temp.isPres ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rp ;                           --"zrobił to", "zrobił ciasto"
        <False,True,False,_> => "nie" ++ qcl.verb ! temp.isPres ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rn ;                 --"nie zrobił tego", "nie zrobił ciasta"
        
        <True,_True,False> => qcl.subj ++ qcl.verb ! temp.isPres ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rp ;                --"kto zrobił ciasto"
        <True,_,False,False> => qcl.subj ++ "nie" ++ qcl.verb ! temp.isPres ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rn ;     --"kto nie zrobił ciasta"

        <False,False,True,False> => "czy" ++ qcl.subj ++ qcl.verb ! temp.isPres ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rp ; --"kot zrobił ciasto"
        <False,False,False,False> => qcl.subj ++ "nie" ++ qcl.verb ! temp.isPres ++ qcl.compl ! qcl.g ! qcl.n ! qcl.rn  --"kot nie zrobił ciasta" 
      }
    } ;

    PredVP np vp = {
      subj = np.s ! Nom ;
      g = np.g ;                     --to determine the gender of the complement
      n = np.n ;                     --to determine the number of the complement
      isPron = np.isPron ;           --whether the subject is a pronoun
      compl = vp.compl ;             --this and the g,n above could also be resolved here since we know the subject already
      complIsPron = vp.complIsPron ; --whether the object is a pronoun
      verb = \\isPres => case <isPres, np.a, np.a2> of { --we do not use auxiliaries to create compound tenses for present or past.

        -- positive/negative/question present: "(nie) pije(?)" (word order does not change, questions are a matter of prosody). 
        <True,NPAgr Sg First,_> => vp.verb.s ! Pres (NPAgr Sg First) ;
        <True,NPAgr Sg Second,_> =>vp.verb.s ! Pres (NPAgr Sg Second) ;
        <True,NPAgr Sg Third,_> => vp.verb.s ! Pres (NPAgr Sg Third) ;
        <True,NPAgr Pl First,_> => vp.verb.s ! Pres (NPAgr Pl First) ;
        <True,NPAgr Pl Second,_> => vp.verb.s ! Pres (NPAgr Pl Second) ;
        <True,NPAgr Pl Third,_> => vp.verb.s ! Pres (NPAgr Pl Third) ;

        -- all verbs, past: "(nie) pił"
        <False,_,NPGAgr Sg First Fem> => vp.verb.s ! Past (NPGAgr Sg First Fem) ;
        <False,_,NPGAgr Sg First Neut> => vp.verb.s ! Past (NPGAgr Sg First Neut) ;
        <False,_,NPGAgr Sg First Masc> => vp.verb.s ! Past (NPGAgr Sg First Masc) ;
        <False,_,NPGAgr Sg First MascAnim> => vp.verb.s ! Past (NPGAgr Sg First MascAnim) ;

        <False,_,NPGAgr Sg Second Fem> => vp.verb.s ! Past (NPGAgr Sg Second Fem) ;
        <False,_,NPGAgr Sg Second Neut> => vp.verb.s ! Past (NPGAgr Sg Second Neut) ;
        <False,_,NPGAgr Sg Second Masc> => vp.verb.s ! Past (NPGAgr Sg Second Masc) ;
        <False,_,NPGAgr Sg Second MascAnim> => vp.verb.s ! Past (NPGAgr Sg Second MascAnim) ;

        <False,_,NPGAgr Sg Third Fem> => vp.verb.s ! Past (NPGAgr Sg Third Fem) ;
        <False,_,NPGAgr Sg Third Neut> => vp.verb.s ! Past (NPGAgr Sg Third Neut) ;
        <False,_,NPGAgr Sg Third Masc> => vp.verb.s ! Past (NPGAgr Sg Third Masc) ;
        <False,_,NPGAgr Sg Third MascAnim> => vp.verb.s ! Past (NPGAgr Sg Third MascAnim) ;

        <False,_,NPGAgr Pl First Fem> => vp.verb.s ! Past (NPGAgr Pl First Fem) ;
        <False,_,NPGAgr Pl First Neut> => vp.verb.s ! Past (NPGAgr Pl First Neut) ;
        <False,_,NPGAgr Pl First Masc> => vp.verb.s ! Past (NPGAgr Pl First Masc) ;
        <False,_,NPGAgr Pl First MascAnim> => vp.verb.s ! Past (NPGAgr Pl First MascAnim) ;

        <False,_,NPGAgr Pl Second Fem> => vp.verb.s ! Past (NPGAgr Pl Second Fem) ;
        <False,_,NPGAgr Pl Second Neut> => vp.verb.s ! Past (NPGAgr Pl Second Neut) ;
        <False,_,NPGAgr Pl Second Masc> => vp.verb.s ! Past (NPGAgr Pl Second Masc) ;
        <False,_,NPGAgr Pl Second MascAnim> => vp.verb.s ! Past (NPGAgr Pl Second MascAnim) ;

        <False,_,NPGAgr Pl Third Fem> => vp.verb.s ! Past (NPGAgr Pl Third Fem) ;
        <False,_,NPGAgr Pl Third Neut> => vp.verb.s ! Past (NPGAgr Pl Third Neut) ;
        <False,_,NPGAgr Pl Third Masc> => vp.verb.s ! Past (NPGAgr Pl Third Masc) ;
        <False,_,NPGAgr Pl Third MascAnim> => vp.verb.s ! Past (NPGAgr Pl Third MascAnim)
      } ;
      rp = vp.rp ;
      rn = vp.rn 
    } ;

    QuestCl cl = cl ** {isWh = False} ;
    --sometimes we might be adding the particle "czy" (whether, if), but this is added in UseQCl.

    QuestVP ip vp = (PredVP ip vp) ** {isWh = True} ; 

    ImpVP vp = {
      s = table {
        --since my compls are NOT just strings, I could have stored it here under another variable; however, because we only do this for 2nd 
        --person singular we can select that form already. It's worth mentioning that negated sentences take the object in Gen, not Acc.
        --here I only account for the masculine form (which only matters when the compl has to agree with the (presumed) subject, so when it is 
        --a subject complement, a rather marginal case), since I also did that stretch for second person pronouns, since in neither situation
        --do I see a good way out that would be within the scope of this project.
        True  => vp.verb.s ! Imp2Sg ++ vp.compl ! Masc ! Sg ! vp.rp ;    
        False => "nie" ++ vp.verb.s ! Imp2Sg ++ vp.compl ! Masc ! Sg ! vp.rn
        }
      } ;

  -- Verb
    UseV v = {
      verb = v ;
      compl = table {g => table {n => table {c => []}}} ; --no object or complement
      complIsPron = False ;
      rp = Nom ;
      rn = Nom ; --this does not really matter here (all entries are identical or empty), but is required by the VP format.
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = table {g => table {n => table {c => v2.cp ++ np.s ! c}}} ; -- NP object needed in Acc and Gen
      complIsPron = np.isPron ;
      rp = v2.rp ;
      rn = v2.rn ; --this is a case where these cases are needed.
      } ;
 
    ComplVS vs s = {
      verb = vs ;
      compl = table {g => table {n => table {c => "że" ++ s.s}}} ; --"że" particle is added
      complIsPron = False ;
      rp = Nom ;
      rn = Nom ;
      } ;

    ComplVV vv vp = {
      verb = vv ;
      compl = table {g => table {n => table {c => vp.verb.s ! Inf ++ vp.compl ! g ! n ! vp.rp}}} ; --the compl gets fitted to the required case
      complIsPron = False ;
      rp = vp.rp ;
      rn = vp.rn ; 
      } ;

    UseComp comp = {
      verb = be_Verb ; --copula verb
      compl = table {g => table {n => table {c => comp.s ! g ! n}}} ; 
      --subject complement, can later select the appropriate form based on gender and number of the subject; this is needed to satisfy certain 
      --requirements for VP structure
      complIsPron = comp.complIsPron ;
      rp = Ins ;
      rn = Ins ; 
      } ;
      
    CompAP ap = {
      s = table {
        g => table {
          n =>  ap.s ! g ! n ! Nom --allows to select the appropriate form based on gender and number of the subject
        }
      } ;
      complIsPron = False
    } ;
      
    CompNP np = {
      s = table {
        g => table {
          n => np.s ! Ins -- this is the case used when subject=object
        }
      } ;
      complIsPron = np.isPron
    } ;
 
    CompAdv adv = {
      s = table {
        g => table {
          n => adv.s
        }
      } ;
      complIsPron = False
    } ;
 
    --for this one it is worth noting that adverbs are very flexible in terms of position and this sentence-final position works well for
    --adverbials like "in a book" or "with them", but not as much for adverbs like "now" or "already", it sounds a bit off, however, I believe
    --that fixing this would be rather complicated and the solution I can think of does not allow for multiple / stacked adverbs.
    AdvVP vp adv = {
      verb = vp.verb ;
      compl = table {g => table {n => table { c => vp.compl ! g ! n ! c ++ adv.s}}} ; --adds an adv to the object or complement
      complIsPron = vp.complIsPron ; 
      rp = vp.rp ; 
      rn = vp.rn
    } ;

  --Noun  
    DetCN det cn = {
      s = table {c => det.s ! cn.g ! c ++ cn.s ! det.n ! c} ;
      a = NPAgr det.n Third ;
      a2 = NPGAgr det.n Third cn.g ;
      g = cn.g ;
      n = det.n ;
      isPron = False --lets decide if this can be dropped when it is a subject
      } ;

    UsePN pn = {
      s = pn.s ! Sg ;
      isPron = False ;
      a = NPAgr Sg Third ;
      a2 = NPGAgr Sg Third pn.g ;
      g = pn.g ;
      n = Sg
      } ;
      
    UsePron p = p ;  

    MassNP cn = {
      s = cn.s ! Sg ;
      isPron = False ;
      a = NPAgr Sg Third ;
      a2 = NPGAgr Sg Third cn.g ;
      g = cn.g ;
      n = Sg 
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
      --here only the MascAnim one is different from the rest
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

  -- Conjunction
    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

  -- Polarity  
    PPos  = {s = [] ; isTrue = True} ;
    PNeg  = {s = [] ; isTrue = False} ;

  -- Temporal
    TSim  = {s = []    ; isPres = True} ;
    TAnt  = {s = []    ; isPres = False} ;

    --these are more or less in free variation, with the first two having a slight difference in formality, and the latter two differing when they
    --are used for logic (OR and XOR), in everyday Polish they are the same. We tend to try to avoid repetition and thus like having many near
    --synonyms. This tendency is only visible in more formal writing though, I do not know if it is displayed in speech as well.
    and_Conj = {s = variants {"i" ; "oraz"}} ;
    or_Conj = {s = variants {"lub" ; "albo" }} ;

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
    --for the with_Prep one case still does not work, which is "with me"="ze mną". This letter combination does not otherwise trigger this form
    --of the preposition, this is an exception.
    with_Prep = {s = pre {("sc"|"sf"|"sg"|"sj"|"sk"|"sl"|"sł"|"sm"|"sn"|"sp"|"sr"|"st"|"sw"|
                          "zb"|"zd"|"zg"|"zj"|"zl"|"zł"|"zm"|"zn"|"zr"|"zs"|"zw"|"zż"|
                          "śc"|"śl"|"śm"|"śn"|"śp"|"śr"|"św"|
                          "źd"|"źl"|"źr"|
                          "żb"|"żd"|"żg"|"żl"|"żł"|"żm"|"żn"|"żr"|"żw"|
                          "rzg"|"rzn"|"rzp"|
                          "szc"|"szk"|"szl"|"szł"|"szm"|"szn"|"szp"|"szr"|"szt"|"szw") => "ze" ; 
                          _ => "z"} ; c = Ins} ; --instrumental

    --For the majority of pronouns there are alternative versions (e.g. for masculine accusative singular there is "jego", "go", "niego", "-ń"); 
    --these have a relatively predictable distribution, but in some contexts they are interchangeable. I think implementing all of them is 
    --beyond the scope of this assignment. The "n" forms appear after prepositions. Short forms (without "je", wherever applicable) can only 
    --appear in unstressed positions (which means what in the SVO sentences they are likely to appear in that form). "-ń" is a clitic of some 
    --prepositions, and only works for the masculine.
    
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
      a2 = NPGAgr Sg First Fem ;
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
      a2 = NPGAgr Sg Second MascAnim ;
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
      a2 = NPGAgr Sg Third MascAnim ;
      g = MascAnim ; 
      --this pronoun would also work for regular Masc
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
      a2 = NPGAgr Sg Third Fem ;
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
      a2 = NPGAgr Pl First Fem ;
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
      a2 = NPGAgr Pl Second MascAnim ;
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
      a2 = NPGAgr Pl Third MascAnim ;
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
      a2 = NPGAgr Sg Third MascAnim ;
      g = MascAnim ;
      n = Sg ;
      isPron = True ;
      } ;

    where_IAdv = {s = "gdzie"} ;
    why_IAdv = {s = variants {"czemu" ; "po co" ; "dlaczego"}} ; --same as above, this only differs slightly in terms of formality

    have_V2 = (mkVerb "mieć" "miej" "mam" "masz" "ma" "mamy" "macie" "mają" "miał" "miał" "miel") ** {cp = [] ; rp = Acc ; rn = Dat} ;

    want_VV = mkVerb "chcieć" "chciej" "chcę" "chcesz" "chce" "chcemy" "chcecie" "chcą" "chciał" "chciał" "chciel" ;
    
}
