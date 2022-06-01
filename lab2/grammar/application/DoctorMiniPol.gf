--# -path=.:../abstract:../english:../api:../mini

-- model implementation using Mini RGL
-- Because of this, one thing is impossible to achieve (noun-noun compounds). I am also aware that since I did the MiniGrammar I did NOT need to
-- make a Doctor one, but I decided to try to figure it out either way. If I must choose, I do not want this to be graded, and would rather have
-- my MiniGrammar be graded only, but if this can be counted as some extra effort, by all means, please do so.

concrete DoctorMiniPol of Doctor =
  open
    MiniGrammarPol,
    MiniParadigmsPol,
    MiniResPol,
    Prelude
  in {

-- application using your own Mini* modules

lincat
  Phrase = Utt ;
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = UttS (UseCl TSim PPos fact) ;
  presNegPhrase fact = UttS (UseCl TSim PNeg fact) ;
  pastPosPhrase fact = UttS (UseCl TAnt PPos fact) ;
  pastNegPhrase fact = UttS (UseCl TAnt PNeg fact) ;
  -- presQuestionPhrase fact = mkUtt (UseQCl (QuestCl fact)) ;
  -- pastQuestionPhrase fact = mkUtt (UseQCl anteriorAnt (QuestCl fact)) ;
  presQuestionPhrase fact = let p : Utt = UttQS (UseQCl TSim PPos (QuestCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = UttQS (UseQCl TAnt PPos (QuestCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;


  impPosPhrase action = UttImpSg PPos (ImpVP action) ;
  impNegPhrase action = UttImpSg PNeg (ImpVP action) ;

  actionFact person action = PredVP person action ;
  propertyFact person property = PredVP person property ;

  isProfessionProperty profession = UseComp (CompNP (DetCN a_Det profession)) ;
  needProfessionProperty profession = ComplV2 need_V2 (DetCN a_Det profession) ;
  isAtPlaceProperty place = UseComp (CompAdv place.at) ;
  haveIllnessProperty illness = ComplV2 have_V2 illness ;

  theProfessionPerson profession = DetCN the_Det profession ;

  iMascPerson = mkNP {
      s = table {
                Nom => "ja" ;
                Gen => "mnie" ;
                Dat => "mi" ; 
                Acc => "mnie" ; 
                Ins => "mną" ;
                Loc => "mnie" ;
                Voc => "ja"
                } ;
      a = NPAgr Sg First ;
      a2 = NPGAgr Sg First MascAnim ;
      g = MascAnim ;
      n = Sg ;
      isPron = True ;
      } ; 
  iFemPerson = mkNP i_Pron ;
  youMascPerson = mkNP youSg_Pron ;
  youFemPerson = mkNP {
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
      a2 = NPGAgr Sg Second Fem ;
      g = Fem ; 
      n = Sg ;
      isPron = True ;
      } ; 
  hePerson = mkNP he_Pron ;
  shePerson = mkNP she_Pron ;

  goToAction place = AdvVP (UseV go_V) place.to ;
  stayAtAction place = AdvVP (UseV stay_V) place.at ;
  vaccinateAction person = ComplV2 vaccinate_V2 person ;
  examineAction person = ComplV2 examine_V2 person ;
  takeSubstanceAction substance = ComplV2 take_V2 substance ; --still returns "take alcohol", but it does so in the English version too from what I can see

-- end of what could be a functor
--------------------------------

  coughAction = UseV (mkV "kaszleć" "kaszl" "kaszlę" "kaszlesz" "kaszle" "kaszlemy" "kaszlecie" "kaszlą" "kaszlał" "kaszlał" "kaszlel") ; --XIth conjugation is not implemented
  breatheAction = UseV (mkV "oddychać" I) ;
  vomitAction = UseV (mkV "wymiotować" IV) ;
  sleepAction = UseV (mkV "spać" "śpij" "śpię" "śpisz" "śpi" "śpimy" "śpicie" "śpią" "spał" "spał" "spal") ; --irregular
  undressAction = ComplV2 takeoff_V2 (mkNP aPl_Det (mkN "ubranie" "ubrania" "ubrań" Neut)) ;
  dressAction = ComplV2 put_V2 (mkNP aPl_Det (mkN "ubranie" "ubrania" "ubrań" Neut)) ;
  eatAction = UseV (mkV "jeść" "jedz" "jem" "jesz" "je" "jemy" "jecie" "jedzą" "jadł" "jadł" "jedl") ; --irregular
  drinkAction = UseV (mkV "pić" Xa) ;
  smokeAction = UseV (mkV "palić" VIa) ;
  --The next two require an explanation. First of all, the words "temperature" and "pressure" in Polish in this context would invariably mean
  --body temperature and blood pressure, and adding the second part would be a bit superfluous. More importantly, I do not find, in the scope
  --of this or my MiniGrammar, an option to express this kind of a noun phrase. This is because unlike in English, where it is the last part
  --of the phrase that gets inflected, here it is the first one. However, unlike in Romance languages, the second part is NOT an adjective, but
  --a noun in the Genitive form. Essentially we are measuring "temperature of the body" and "pressure of the blood". I have tried some different
  --ways to make this work but without editing the abstract grammar for either part of the assignment, I cannot add a function PossNP that would
  --let me create these possessive phrases, unfortunately. Thus, I left it at only the core "meaningful" word of the compound.
  measureTemperatureAction = ComplV2 (mkV2 (mkV "mierzyć" VIb) Acc Gen) (mkNP a_Det (mkN "temperatura" "temperatury" "temperatur" Fem)) ; --ciała
  measureBloodPressureAction = ComplV2 (mkV2 (mkV "mierzyć" VIb) Acc Gen) (mkNP a_Det (mkN "ciśnienie" "ciśnienia" "ciśnień" Neut)) ; --krwi

  hospitalPlace = {at = PrepNP in_Prep (mkNP a_Det (mkN "szpital" "szpitale" "szpitali" Masc)) ; to = PrepNP (mkPrep "do" Gen) (mkNP a_Det (mkN "szpital" "szpitale" "szpitali" Masc))} ;
  homePlace = {at = PrepNP in_Prep (mkNP a_Det (mkN "dom" "domy" "domów" Masc)) ; to = PrepNP (mkPrep "do" Gen) (mkNP a_Det (mkN "dom" "domy" "domów" Masc))} ;
  schoolPlace = {at = PrepNP in_Prep (mkNP a_Det (mkN "szkoła" "szkoły" "szkół" Fem)) ; to = PrepNP (mkPrep "do" Gen) (mkNP a_Det (mkN "szkoła" "szkoły" "szkół" Fem))} ;
  workPlace = {at = PrepNP in_Prep (mkNP a_Det (mkN "praca" "prace" "prac" Fem)) ; to = PrepNP (mkPrep "do" Gen) (mkNP a_Det (mkN "praca" "prace" "prac" Fem))} ;

  doctorProfession = UseN (mkN "lekarka" "lekarki" "lekarek" Fem) ; --or "lekarz"/"doktor" for MascAnim, but I decided to go with a feminine form
  nurseProfession = UseN (mkN "pielęgniarz" "pielęgniarze" "pielęgniarzy" MascAnim) ; --or "pielęgniarka" for Fem
  interpreterProfession = UseN (mkN "tłumaczka" "tłumaczki" "tłumaczek" Fem) ; --or "tłumacz" for MascAnim

  bePregnantProperty = UseComp (CompAP (PositA (mkA "ciężarny"))) ;
  beIllProperty = UseComp (CompAP (PositA (mkA "chory"))) ;
  beWellProperty = UseComp (CompAP (PositA (mkA "zdrowy"))) ;
  beDeadProperty = UseComp (CompAP (PositA (mkA "martwy"))) ;
  haveAllergiesProperty = ComplV2 have_V2 (mkNP a_Det (mkN "alergia" "alergie" "alergii" Fem)) ;
  havePainsProperty = ComplV2 have_V2 (mkNP a_Det (mkN "ból" "bóle" "bóli" Masc)) ; --another, slightly more natural way to say it would be "it hurts me" / "boli mnie", but that would be a bit harder to implement (the subjects are restricted to pronouns or body parts that can hurt etc.)
  haveChildrenProperty = ComplV2 have_V2 (mkNP a_Det (mkN "dziecko" "dziecka" "dziecku" "dziecko" "dzieckiem" "dziecku" "dziecko" "dzieci" "dzieci" "dzieciom" "dzieci" "dziećmi" "dzieciach" "dzieci" Neut)) ; 

  feverIllness = mkNP a_Det (mkN "gorączka" "gorączki" "gorączek" Fem) ;
  fluIllness = mkNP a_Det (mkN "grypa" "grypy" "gryp" Fem) ;
  headacheIllness = mkNP a_Det (mkN "ból" "bóle" "bóli" Masc) ; --głowy
  diarrheaIllness = mkNP a_Det (mkN "biegunka" "biegunki" "biegunek" Fem) ;
  heartDiseaseIllness = mkNP a_Det (mkN "choroba" "choroby" "chorób" Fem) ; --serca
  lungDiseaseIllness = mkNP a_Det (mkN "choroba" "choroby" "chorób" Fem) ; --płuc
  hypertensionIllness = mkNP a_Det (mkN "nadciśnienie" "nadciśnienia" "nadciśnień" Neut) ;

  alcoholSubstance = mkNP aPl_Det (mkN "alkohol" "alkohole" "alkoholi" Masc) ;
  medicineSubstance = mkNP the_Det (mkN "lekarstwo" "lekarstwa" "lekarstw" Neut) ;
  drugsSubstance = mkNP thePl_Det (mkN "narkotyk" "narkotyki" "narkotyków" Masc) ;

oper

  go_V = mkV "iść" "idź" "idę" "idziesz" "idzie" "idziemy" "idziecie" "idą" "szł" "szedł" "szl" ; --irregular
  stay_V = mkV "zostać" "zostań" "zostaję" "zostajesz" "zostaje" "zostajemy" "zostajecie" "zostają" "został" "został" "zostal" ; --irregular, present and past are in different aspects (imperf in present, perf in past)
  need_V2 = mkV2 (mkV "potrzebować" IV) Acc Gen ;
  take_V2 = mkV2 (mkV "brać" "bierz" "biorę" "bierzesz" "bierze" "bierzemy" "bierzecie" "biorą" "brał" "brał" "bral") Acc Gen ; 
  --the next three, same as with stay, mix two different aspects for the two different tenses; it's worth noting that we have entirely different 
  --verbs for different aspects, not just different endings.
  takeoff_V2 = mkV2 (mkV "zdjąć" "zdejmij" "zdejmuję" "zdejmujesz" "zdejmuje" "zdejmujemy" "zdejmujecie" "zdejmują" "zdejmował" "zdejmował" "zdejmowal") Acc Gen ; 
  put_V2 = mkV2 (mkV "założyć" "załóż" "zakładam" "zakładasz" "zakłada" "zakładamy" "zakładacie" "zakłada" "założył" "założył" "założyl") Acc Gen ; --załóż
  vaccinate_V2 = mkV2 (mkV "zaszczepić" "zaszczep" "szczepię" "szczepisz" "szczepi" "szczepimy" "szczepicie" "szczepią" "zaszczepił" "zaszczepił" "zaszczepil") Acc Gen ;
  examine_V2 = mkV2 (mkV "badać" I) Acc Gen ;

  --This is here to make it easier to make NPs
  mkNP = overload {
    mkNP : Det -> Noun -> NP 
      = \det, noun -> lin NP (DetCN det (UseN noun)) ;
    mkNP : Pron -> NP
      = \pron -> lin NP (UsePron pron)
  } ;

}
