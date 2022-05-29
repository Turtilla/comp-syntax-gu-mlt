resource MiniResPol = open Prelude in {

param
  VForm = Inf | Imp2Sg | Pres NPAgreement | Past NPGAgreement ;
  Number = Sg | Pl ;
  Case = Nom | Gen | Dat | Acc | Ins | Loc | Voc ;
  Gender = MascAnim | Masc | Fem | Neut ;
  Person = First | Second | Third ;
  Conjugation = I | II | III | IV | Va | Vb | Vc | VIa | VIb | VIIa | VIIb | VIIIa | VIIIb | IX | Xa | Xb ; -- | Xc | XI not implemented because of irregularities

  NPAgreement = NPAgr Number Person ; --needed for present verbs
  NPGAgreement = NPGAgr Number Person Gender ; --needed for past verbs

oper

  --For nouns I need to store 2 numbers, 7 cases each, and the gender. Aside from the typical masculine, feminine, neuter, there is something that
  --I here called the masculine animate gender, but it mostly sets apart human beings from all the other masculine nouns. The major challenge here
  --was that aside from these categories, the final consonant of the root of the noun also influences the kinds of endings or sound changes that it
  --undergoes. In addition, sometimes there is a case-to-case ending variation that is not dictated by any features of the word, but is simply the
  --more widely accepted form, and there is no predicting it without knowing it beforehand. This is why this is not an exhaustive list. Due to there
  --often being animate and inanimate forms of the same masculine noun, I needed to introduce the plural as a variable that would help me distinguish
  --between those; it also helps tell apart historically soft p, b, m, w from their hard counterparts (which is not visible in the nominative singular)
  --for masculine nouns. From what I saw the official Polish grammar handles this by having tens, if not hundreds, of sets of endings, and it passes
  --nouns to the function together with a number to identify the set. I wanted to avoid it, but, consequently, my smart noun function is not ideal
  --and cannot account for all the forms, especially the ones that are in free variation or undergo some not fully predictable internal sound changes.
  --I also chose not to implement some patterns that I know only concern one or two words, as these seem to be irregular enough to be treated as such.
  --There is a smart function for each gender, since the sheer number of variations made it hard to otherwise troubleshoot errors (it is easier to
  --fix the error if you know that it is in the endings for masculine nouns, than for all the nouns). There is then a "master function" smartNoun,
  --which selects which of the four to use based on the given gender.

  --Requiring plural genitive solved a lot of other issues that forced me to split a lot of variants: for instance, in many cases words with an
  --internal "o" have that vowel shift to "ó" in genitive plural. This let me limit a number of otherwise very intricate "case of" expressions
  --that would have nearly doubled the number of paradigms had I wanted this vowel change to work without requiring the plural genitive. The plural
  --nominative is needed to tell apart some paradigms, and potentially not all sound changes are accounted for, as my sources were not extremely
  --comprehensive with describing when they do.

  Noun : Type = {s : Number => Case => Str ; g : Gender} ;

  mkNoun : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> Noun 
        = \sgnom, sggen, sgdat, sgacc, sgins, sgloc, sgvoc, plnom, plgen, pldat, placc, plins, plloc, plvoc, g -> {
        s = table {
            Sg => table {
                Nom => sgnom ;
                Gen => sggen ;
                Dat => sgdat ;
                Acc => sgacc ;
                Ins => sgins ;
                Loc => sgloc ;
                Voc => sgvoc
            } ;
            Pl => table {
                Nom => plnom ;
                Gen => plgen ;
                Dat => pldat ;
                Acc => placc ;
                Ins => plins ;
                Loc => plloc ;
                Voc => plvoc 
            }
            } ;
            g = g
        } ;

  smartNounNeut : Str -> Str -> Str -> Gender -> Noun = \noun,plural,gen,g -> case noun of {
              ---most neuter nouns that end with "o", like "jajo" or "mleko"; does not account for the relatively irregular "dziecko"
              jaj + "o" => case gen of {
                mi + "ast" => mkNoun noun plural (jaj + "u") noun (jaj + "em") (mi + "eście") noun plural gen (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g ; --miasto, miasta, miast
                jezio + "r" => mkNoun noun plural (jaj + "u") noun (jaj + "em") (jezio + "rze") noun plural gen (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g ; --jezioro, jeziora, jezior
                sl + "ow" => mkNoun noun plural (jaj + "u") noun (jaj + "em") (jaj + "ie") noun plural gen (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g ; --słowo, słowa, słów
                _ => mkNoun noun plural (jaj + "u") noun (jaj + "em") (jaj + "u") noun plural gen (jaj + "om") plural (jaj + "ami") (jaj + "ach") plural g --jajo, jaja, jaj or jajko, jajka, jajek
              } ;
              --foreign neuter nouns that end with "um", where all the singular forms are the same, e.g. "muzeum", "memorandum".
              muze + "um" => mkNoun noun noun noun noun noun noun noun plural gen (muze + "om") plural (muze + "ami") (muze + "ach") plural g ; --muzeum, muzea, muzeów
              --neuter nouns ending with "ę" that mostly denote the young of some species (e.g. "cielę" - calf, "prosię" - piglet). Does not account for "imię".
              zwierz + "ę" => mkNoun noun (noun + "cia") (noun + "ciu") noun (noun + "ciem") (noun + "ciu") noun plural gen (noun + "tom") plural (noun + "tami") (noun + "tach") plural g ; --zwierzę, zwierzęta, zwierząt
              --neuter nouns ending in "e", with a case accounting for internal vowel mutation.
              pol + "e" => mkNoun noun plural (pol + "u") (pol + "e") (pol + "em") (pol + "u") (pol + "e") plural gen (pol + "om") plural (pol + "ami") (pol + "ach") plural g ; --pole, pola, pól
              --neuter nouns ending with "us".
              op + "us" => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "ie") (noun + "ie") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g --opus, opusy, opusów
  } ;

  smartNounFem : Str -> Str -> Str -> Gender -> Noun = \noun,plural,gen,g -> case noun of { 
              --feminine nouns ending in "ew" - the plurals for "krew" (blood) are not really used and the ones here are modelled after "brew" (eyebrow), and don't fully fit the dictionary entry for "krew", but sound acceptable to me as a native speaker.
              kr + "ew" => mkNoun noun gen gen noun (kr + "wią") gen gen plural gen (kr + "wiom") gen (kr + "wiami") (kr + "wiach") plural g ; --krew, krwie, krwi or brew, brwi, brwi
              --feminine nouns ending with "a" with a velar stem.
              wal + "ka" => mkNoun noun plural (wal + "ce") (wal + "kę") (wal + "ką") (wal + "ce") (wal + "ko") plural gen (wal + "kom") plural (wal + "kami") (wal + "kach") plural g ; --walka, walki, walk
              dro + "ga" => mkNoun noun plural (dro + "dze") (dro + "gę") (dro + "gą") (dro + "dze") (dro + "go") plural gen (dro + "gom") plural (dro + "gami") (dro + "gach") plural g ; --droga, drogi, dróg
              ce + "cha" => mkNoun noun plural (ce + "sze") (ce + "chę") (ce + "chą") (ce + "sze") (ce + "cho") plural gen (ce + "chom") plural (ce + "chami") (ce + "chach") plural g ; --cecha, cechy, cech 
              wata + "ha" => mkNoun noun plural (wata + "sze") (wata + "hę") (wata + "hą") (wata + "sze") (wata + "ho") plural gen (wata + "hom") plural (wata + "hami") (wata + "hach") plural g ; --wataha, watahy, watah
              --feminine nouns ending with "a" with a historically soft stem.
              pra + ("ca"|"dza"|"sza"|"ża"|"rza"|"cza"|"dża") => case noun of {prac + "a" => mkNoun noun (prac + "y") (prac + "y") (prac + "ę") (prac + "ą") (prac + "y") (prac + "o") plural gen (prac + "om") plural (prac + "ami") (prac + "ach") plural g } ; --praca, prace, prac or rdza, rdze, rdzy
              --feminine nouns ending with "a" with a soft stem; does not account for some animate nouns' vocatives in singular ending with "u" instead of "o"; I split them into two based on where the majority of the nouns have the "o" or the "u", but there are still exceptions.
              a + ("la"|"bia"|"mia"|"wia"|"pia"|"ia") => case noun of {asi + "a" => mkNoun noun asi asi (asi + "ę") (asi + "ą") asi (asi + "o") plural gen (asi + "om") plural (asi + "ami") (asi + "ach") plural g } ; --Asia, Asie, Aś
              a + ("sia"|"cia"|"zia"|"dzia"|"nia") => case noun of {asi + "a" => mkNoun noun asi asi (asi + "ę") (asi + "ą") asi (asi + "u") plural gen (asi + "om") plural (asi + "ami") (asi + "ach") plural g } ; --babcia, babcie, babć
              fre + "ja" => mkNoun noun gen gen (fre + "ję") (fre + "ją") gen (fre + "jo") plural gen (fre + "jom") plural (fre + "jami") (fre + "jach") plural g ; --Freja, Freje, Frei
              --feminine nouns ending with "a" with a hard stem.
              la + ("ba"|"pa"|"fa"|"wa"|"ma"|"ła"|"sa"|"za"|"na") => case noun of {lab + "a" => mkNoun noun plural (lab + "ie") (lab + "ę") (lab + "ą") (lab + "ie") (lab + "o") plural gen (lab + "om") plural (lab + "ami") (lab + "ach") plural g} ; --laba, laby, lab
              la + "ta" => mkNoun noun plural (la + "cie") (la + "tę") (la + "tą") (la + "cie") (la + "to") plural gen (la + "tom") plural (la + "tami") (la + "tach") plural g ; --łata, łaty, łat
              wo + "da" => mkNoun noun plural (wo+ "dzie") (wo + "dę") (wo + "dą") (wo + "dzie") (wo + "do") plural gen (wo + "dom") plural (wo + "dami") (wo + "dach") plural g ; --woda, wody, wód
              kobie + "ta" => mkNoun noun plural (kobie + "cie") (kobie + "tę") (kobie + "tą") (kobie + "cie") (kobie + "to") plural gen (kobie + "tom") plural (kobie + "tami") (kobie + "tach") plural g ; --kobieta, kobiety, kobiet
              szko + "ła" => mkNoun noun plural (szko + "le") (szko + "łę") (szko + "łą") (szko + "le") (szko + "ło") plural gen (szko + "łom") plural (szko + "łami") (szko + "łach") plural g ; --szkoła, szkoły, szkół
              ope + "ra" => mkNoun noun plural (ope + "rze") (ope + "rę") (ope + "rą") (ope + "rze") (ope + "ro") plural gen (ope + "rom") plural (ope + "rami") (ope + "rach") plural g ; --opera, opery, oper
              --feminine nouns ending with "a" with a vowel stem.
              id + "ea" => case noun of { ide + "a" => mkNoun noun gen gen (ide + "ę") (ide + "ą") gen (ide + "o") plural gen (ide + "om") plural (ide + "ami") (ide + "ach") plural g } ; --idea, idee, idei
              stat + "ua" => case noun of { statu + "a" => mkNoun noun plural gen (statu + "ę") (statu + "ą") gen (statu + "o") plural gen (statu + "om") plural (statu + "ami") (statu + "ach") plural g } ; --statua, statuy, statui
              --feminine nouns ending with "i".
              mistrzy + "ni" => mkNoun noun noun noun (mistrzy + "nię") (mistrzy + "nią") noun noun plural gen (mistrzy + "niom") plural (mistrzy + "niami") (mistrzy + "niach") plural g ; --mistrzyni, mistrzynie, mistrzyń
              --feminine nouns ending with a historically soft consonant.
              podro + ("c"|"dz"|"sz"|"ż"|"rz"|"cz"|"dż") => mkNoun noun gen gen noun (noun + "ą") gen gen plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --podróż, podróże, podróży
              --feminine nouns ending with a soft consonant.
              milo + "ść" => mkNoun noun plural plural noun (milo + "ścią") plural plural plural plural (milo + "ściom") plural (milo + "ściami") (milo + "ściach") plural g ; --miłość, miłości, miłości
              ja + "źń" => mkNoun noun gen gen noun (ja + "źnią") gen gen plural gen (ja + "źniom") plural (ja + "źniami") (ja + "źniach") plural g --jaźń, jaźnie, jaźni
   } ;

  smartNounMasc : Str -> Str -> Str -> Gender -> Noun = \noun,plural,gen,g -> case noun of {
              --velar stem masculine nouns; does not account for irregular variation of "a" and "u" in genitive singular.
              par + ("k"|"g") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "iem") (noun + "u") (noun + "u") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --park, parki, parków
              da + ("h"|"ch") => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --dch, dachy, dachów
              --historically soft stem masculine nouns.
              --add other variants
              zaj + "ąc" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --zając, zające, zajęcy
              klu + ("cz"|"sz"|"ż"|"rz"|"dż"|"c"|"dz") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --klucz, klucze, kluczy
              --soft stem masculine nouns.
              ki + ("j"|"l") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --kij, kije, kijów
              ko + "ń" => mkNoun noun (ko + "nia") (ko + "niowi") (ko + "nia") (ko + "niem") (ko + "niu") (ko + "niu") plural gen (ko + "niom") plural (ko + "niami") (ko + "niach") plural g ; --koń, konie, koni
              pa + "ź" => mkNoun noun (pa + "zia") (pa + "ziowi") (pa + "zia") (pa + "ziem") (pa + "ziu") (pa + "ziu") plural gen (pa + "ziom") plural (pa + "ziami") (pa + "ziach") plural g ; --paź, pazie, pazi
              re + "dź" => mkNoun noun (re + "dzia") (re + "dziowi") (re + "dzia") (re + "dziem") (re + "dziu") (re + "dziu") plural gen (re + "dziom") plural (re + "dziami") (re + "dziach") plural g ;
              mi + "ś" => mkNoun noun (mi + "sia") (mi + "siowi") (mi + "sia") (mi + "siem") (mi + "siu") (mi + "siu") plural gen (mi + "siom") plural (mi + "siami") (mi + "siach") plural g ; --miś, misie, misiów
              ki + "ć" => mkNoun noun (ki + "cia") (ki + "ciowi") (ki + "cia") (ki + "ciem") (ki + "ciu") (ki + "ciu") plural gen (ki + "ciom") plural (ki + "ciami") (ki + "ciach") plural g ;
              --some soft stem masculine nouns and hard stem masculine nouns (they are indistinguishable in Nom Sg), need a "case plural of" expression.
              kar + ("p"|"b"|"m"|"w") => case plural of {
                kar + ("pie"|"bie"|"mie"|"wie") => mkNoun noun (noun + "ia") (noun + "iowi") (noun + "ia") (noun + "iem") (noun + "iu") (noun + "iu") plural gen (noun + "iom") plural (noun + "iami") (noun + "iach") plural g ; --karp, karpie, karpi
                --hard stem masculine nouns.
                --these two below help with some free variation but there are exceptions to this rule.
                sta + ("by"|"my"|"wy") => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "ie") (noun + "ie") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --staw, stawy,stawów
                do + "my" => mkNoun noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g --dom, domy, domów
              } ;
              rowe + "r" => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "ze") (noun + "ze") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --rower, rowery, rowerów
              fia + "t" => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (fia + "cie") (fia + "cie") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --fiat, fiaty, fiatów
              --this example showcases well the issues with local irregularities; the same word in the nominative means mud/river sand or a mule. These endings are for the animal, but the mud version gets a different genitive and accusative singular ("u" and no ending). There is no regularity to this, nor is there a way to predict it other than just knowing.
              mu + "ł" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (mu + "le") (mu + "le") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --muł, muły, mułów
              ga + "d" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (ga + "dzie") (ga + "dzie") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g ; --gad, gady, gadów
              no + ("n"|"z"|"s"|"f") => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "ie") (noun + "ie") plural gen (noun + "om") plural (noun + "ami") (noun + "ach") plural g --nos, nosy, nosów
              --due to their rarity or difficulty, some masculine nouns with fleeting vowels (ogień vs. ognie), with the "ą" vowel (wąż vs. węży), and full irregulars have been excluded.
  } ;

  smartNounMascAnim : Str -> Str -> Str -> Gender -> Noun = \noun,plural,gen,g -> case plural of {
              --masculine animate nouns ending with "owie" in Nom Pl.
              krol + "owie" => case noun of { 
                zie + "ć" => mkNoun noun (zie + "cia") (zie + "ciowi") (zie + "cia") (zie + "ciem") (zie + "ciu") (zie + "ciu") plural gen (zie + "ciom") (zie + "ciów") (zie + "ciami") (zie + "ciach") plural g ; --zięć, zięciowie, zięciów
                zie + "ś" => mkNoun noun (zie + "sia") (zie + "siowi") (zie + "sia") (zie + "siem") (zie + "siu") (zie + "siu") plural gen (zie + "siom") (zie + "siów") (zie + "siami") (zie + "siach") plural g ;
                zie + "ń" => mkNoun noun (zie + "nia") (zie + "niowi") (zie + "nia") (zie + "niem") (zie + "niu") (zie + "niu") plural gen (zie + "niom") (zie + "niów") (zie + "niami") (zie + "niach") plural g ;
                zie + "ź" => mkNoun noun (zie + "zia") (zie + "ziowi") (zie + "zia") (zie + "ziem") (zie + "ziu") (zie + "ziu") plural gen (zie + "ziom") (zie + "ziów") (zie + "ziami") (zie + "ziach") plural g ;
                zie + "dź" => mkNoun noun (zie + "dzia") (zie + "dziowi") (zie + "dzia") (zie + "dziem") (zie + "dziu") (zie + "dziu") plural gen (zie + "dziom") (zie + "dziów") (zie + "dziami") (zie + "dziach") plural g ;
                ja + ("n"|"m") => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") plural gen (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (noun + "owie") g ; --Jan, Janowie, Janów
                krol => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") (noun + "ów") (noun + "ami") (noun + "ach") (noun + "owie") g --król, królowie, królów
               } ;
              --masculine animate nouns ending with "i" in Nom Pl.
              adwokac + "i" => case noun of {
                angli + "sta" => mkNoun noun (angli + "sty") (angli + "ście") (angli + "stę") (angli + "stą") (angli + "ście") (angli + "sto") plural gen (angli + "stom") gen (angli + "stami") (angli + "stach") (angli + "ści") g ; --anglista, angliści, anglistów
                poe + "ta" => mkNoun noun (poe + "ty") (poe + "cie") (poe + "tę") (poe + "tą") (poe + "cie") (poe + "to") plural gen (poe + "tom") gen (poe + "tami") (poe + "tach") (poe + "ci") g ; --poeta, poeci, poetów
                meszczy + "zna" => mkNoun noun (meszczy + "zny") (meszczy + "źnie") (meszczy + "znę") (meszczy + "zną") (meszczy + "źnie") (meszczy + "zno") plural gen (meszczy + "znom") gen (meszczy + "znami") (meszczy + "znach") (meszczy + "źni") g ; --mężczyzna, mężczyźni, mężczyzn
                adwoka + "t" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (adwoka + "cie") (adwoka + "cie") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") (adwoka + "ci") g ; --adwokat, adwokaci, adwokatów
                cze + "ch" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") (cze + "si") g ; --Czech, Czesi, Czechów
                sasi + "ad" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (sasi + "edzie") (sasi + "edzie") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") (sasi + "edzi") g ; --sąsiad, sąsiedzi, sąsiadów
                szwe + "d" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "zie") (noun + "zie") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") (noun + "zi") g ; --Szwed, Szwedzi, Szwedów
                weteran => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") (noun + "i") g --weteran, weterani, weteranów
              } ;
              --masculine animate nouns ending with "y" in Nom Pl.
              koledz + "y" => case noun of {
                kole + "ga" => mkNoun noun (kole + "gi") (kole + "dze") (kole + "gę") (kole + "gą") (kole + "dze") (kole + "go") plural gen (kole + "gom") gen (kole + "gami") (kole + "gach") plural g ; --kolega, koledzy, kolegów
                doradc + "a" => mkNoun noun plural plural (doradc + "ę") (doradc + "ą") plural (doradc + "o") plural gen (doradc + "om") gen (doradc + "ami") (doradc + "ach") plural g ; --doradca, doradcy, doradców
                anglik => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "iem") (noun + "u") (noun + "u") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") plural g --Anglik, Anglicy, Anglików
              } ;
              --masculine animate nouns ending with "e" in Nom Pl.
              gosci + "e" => case noun of {
                gos + "ć" => mkNoun noun (gos + "cia") (gos + "ciowi") (gos + "cia") (gos + "ciem") (gos + "ciu") (gos + "ciu") plural gen (gos + "ciom") gen (gos + "ciami") (gos + "iach") plural g ; --gość, goście, gości
                cesarz => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "u") (noun + "u") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") plural g --cesarz, cesarze, cesarzy
              } ;
              --masculine animate nouns ending with "anie" in Nom Pl.
              ameryk + "anie" => case noun of {
                ameryk + "anin" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") plural gen (ameryk + "anom") gen (ameryk + "anami") (ameryk + "anach") (ameryk + "anie") g ; --Amerykanin, Amerykanie, Amerykanów
                hiszp + "an" => mkNoun noun (noun + "a") (noun + "owi") (noun + "a") (noun + "em") (noun + "ie") (noun + "ie") plural gen (noun + "om") gen (noun + "ami") (noun + "ach") (noun + "ie") g --Hiszpan, Hiszpanie, Hiszpanów 
              }
  } ;

  smartNoun : Str -> Str -> Str -> Gender -> Noun = \noun,plural,gen,g -> case g of {
            --this was suggested by Arianna to easier troubleshoot problems with the functions for each gender, and to make it more modular
            Neut => smartNounNeut noun plural gen g ;
            Fem => smartNounFem noun plural gen g ;
            Masc => smartNounMasc noun plural gen g ;
            MascAnim => smartNounMascAnim noun plural gen g 
        } ;

  --for adjectives, since this grammar does not include different degrees (nor negated adjectives, which in Polish are spelled together with the
  --negation, e.g. "miły" (nice) vs. "niemiły" (not nice, rude)), I needed to store 4 genders, each with a plural - although the plurals for all
  --but masculine animate/human are the same. There is, in fact, only 11 different forms that a single adjective can have. However, they can differ
  --based on the final vowel in the masculine nominative singular, as well as the final consonant of the root. I decided to tell these apart in the 
  --smart adjective function, and in the make adjective one simply require the 11 forms to be entered. The smart adjective function only needs the
  --masculine singular nominative form as the rest is practically fully predictable from that. This is perhaps the most regular of the categories.
  Adjective : Type = {s : Gender => Number => Case => Str} ;

  mkAdj : (yform, egoform, emuform, ymform, ychform, ymiform, eform, aform, ejform, anform, plmasc : Str) -> Adjective
    = \yform,egoform,emuform,ymform,ychform,ymiform,eform,aform,ejform,anform,plmasc -> {
    s = table {
        MascAnim => table {
          Sg => table {
            Nom => yform ; --żółty, niebieski
            Gen => egoform ; --żółtego, niebieskiego
            Dat => emuform ; --żółtemu, niebieskiemu
            Acc => egoform ; 
            Ins => ymform ; --żółtym, niebieskim
            Loc => ymform ;
            Voc => yform 
          } ;
          Pl => table {
            Nom => plmasc ; --żółci, niebiescy
            Gen => ychform ; --żółtych, niebieskich
            Dat => ymform ;
            Acc => ychform ;
            Ins => ymiform ; --żółtymi, niebieskimi
            Loc => ychform ;
            Voc => plmasc
          }
        } ;
        Masc => table {
          Sg => table {
            Nom => yform ;
            Gen => egoform ;
            Dat => emuform ;
            Acc => yform ; 
            Ins => ymform ;
            Loc => ymform ;
            Voc => yform 
          } ;
          Pl => table {
            Nom => eform ; --żółte, niebieskie
            Gen => ychform ;
            Dat => ymform ;
            Acc => eform ;
            Ins => ymiform ;
            Loc => ychform ;
            Voc => eform
          }
        } ;
        Fem => table {
          Sg => table {
            Nom => aform ; --żółta, niebieska
            Gen => ejform ; --żółtej, niebieskiej
            Dat => ejform ;
            Acc => anform ; --żółtą, niebieską
            Ins => anform ;
            Loc => ejform ;
            Voc => aform
          } ;
          Pl => table {
            Nom => eform ;
            Gen => ychform ;
            Dat => ymform ;
            Acc => eform ;
            Ins => ymiform ;
            Loc => ychform ;
            Voc => eform
          }
        } ;
        Neut => table {
          Sg => table {
            Nom => eform ;
            Gen => egoform ;
            Dat => emuform ;
            Acc => eform ;
            Ins => ymform ;
            Loc => ymform ;
            Voc => eform
          } ;
          Pl => table {
            Nom => eform ;
            Gen => ychform ;
            Dat => ymform ;
            Acc => eform ;
            Ins => ymiform ;
            Loc => ychform ;
            Voc => eform
          }
        } 
      } 
    } ;
  
  
  smartAdj : Str -> Adjective = \masc -> case masc of {
    --the variety of options here stems mostly from MascAnim plural nominative having an affinity for unusual sound changes to the stem.
     du  +  "ży" => mkAdj (du + "ży") (du + "żego") (du + "żemu") (du + "żym") (du + "żych") (du + "żymi") (du + "że") (du + "ża") (du + "żej") (du + "żą") (du + "zi") ;
     wes  +  "oły" => mkAdj (wes + "oły") (wes + "ołego") (wes + "ołemu") (wes + "ołym") (wes + "ołych") (wes + "ołymi") (wes + "ołe") (wes + "oła") (wes + "ołej") (wes + "ołą") (wes + "eli") ;
     mi  +  "ły" => mkAdj (mi + "ły") (mi + "łego") (mi + "łemu") (mi + "łym") (mi + "łych") (mi + "łymi") (mi + "łe") (mi + "ła") (mi + "łej") (mi + "łą") (mi + "li") ;
     mlo  +  "dy" => mkAdj (mlo + "dy") (mlo + "dego") (mlo + "demu") (mlo + "dym") (mlo + "dych") (mlo + "dymi") (mlo + "de") (mlo + "da") (mlo + "dej") (mlo + "dą") (mlo + "dzi") ;
     pro  +  "sty" => mkAdj (pro + "sty") (pro + "stego") (pro + "stemu") (pro + "stym") (pro + "stych") (pro + "stymi") (pro + "ste") (pro + "sta") (pro + "stej") (pro + "stą") (pro + "ści") ;
     pracowi  +  "ty" => mkAdj (pracowi + "ty") (pracowi + "tego") (pracowi + "temu") (pracowi + "tym") (pracowi + "tych") (pracowi + "tymi") (pracowi + "te") (pracowi + "ta") (pracowi + "tej") (pracowi + "tą") (pracowi + "ci") ;
     star  +  "szy" => mkAdj (star + "szy") (star + "szego") (star + "szemu") (star + "szym") (star + "szych") (star + "szymi") (star + "sze") (star + "sza") (star + "szej") (star + "szą") (star + "si") ;
     zmecz  +  "ony" => mkAdj (zmecz + "ony") (zmecz + "onego") (zmecz + "onemu") (zmecz + "onym") (zmecz + "onych") (zmecz + "onymi") (zmecz + "one") (zmecz + "ona") (zmecz + "onej") (zmecz + "oną") (zmecz + "eni") ;
     dob  +  "ry" => mkAdj (dob + "ry") (dob + "rego") (dob + "remu") (dob + "rym") (dob + "rych") (dob + "rymi") (dob + "re") (dob + "ra") (dob + "rej") (dob + "rą") (dob + "rzy") ;
     elegan  +  "cki" => mkAdj (elegan + "cki") (elegan + "ckiego") (elegan + "ckiemu") (elegan + "ckim") (elegan + "ckich") (elegan + "ckimi") (elegan + "ckie") (elegan + "cka") (elegan + "ckiej") (elegan + "cką") (elegan + "ccy") ;
     wyso  +  "ki" => mkAdj (wyso + "ki") (wyso + "kiego") (wyso + "kiemu") (wyso + "kim") (wyso + "kich") (wyso + "kimi") (wyso + "kie") (wyso + "ka") (wyso + "kiej") (wyso + "ką") (wyso + "cy")  ;
     ubo  +  "gi"  => mkAdj (ubo + "gi") (ubo + "giego") (ubo + "giemu") (ubo + "gim") (ubo + "gich") (ubo + "gimi") (ubo + "gie") (ubo + "ga") (ubo + "giej") (ubo + "gą") (ubo + "dzy") ;
     pachna  +  "cy"  => mkAdj (pachna + "cy") (pachna + "cego") (pachna + "cemu") (pachna + "cym") (pachna + "cych") (pachna + "cymi") (pachna + "ce") (pachna + "ca") (pachna + "cej") (pachna + "cą") (pachna + "cy") ;
     proro  +  "czy"  => mkAdj (proro + "czy") (proro + "czego") (proro + "czemu") (proro + "czym") (proro + "czych") (proro + "czymi") (proro + "cze") (proro + "cza") (proro + "czej") (proro + "czą") (proro + "czy") ;
     cu  +  "dzy"  => mkAdj (cu + "dzy") (cu + "dzego") (cu + "dzemu") (cu + "dzym") (cu + "dzych") (cu + "dzymi") (cu + "dze") (cu + "dza") (cu + "dzej") (cu + "dzą") (cu + "dzy") ;
     slab  +  "y" => mkAdj (slab + "y") (slab + "ego") (slab + "emu") (slab + "ym") (slab + "ych") (slab + "ymi") (slab + "e") (slab + "a") (slab + "ej") (slab + "ą")  (slab + "i") ;
     tan + "i" => mkAdj (tan + "i") (tan + "iego") (tan + "iemu") (tan + "im") (tan + "ich") (tan + "imi") (tan + "ie") (tan + "ia") (tan + "iej") (tan + "ią") (tan + "i") 
     } ;

  --for the verb, as far as only the present tense is concerned, only the infinitive and six present verb forms are needed. These are stored using
  --the NPAgr, since they need to agree with the subject in the number and person. In the past tense, in all the forms gender is also something 
  --that needs to be agreed for, and why I utilized NPGAgr. However, as the past tense is, arguably, not a tense but a participle with affixed
  --auxiliaries (lubiłem = jam lubił = ja żem lubił, lubiłeś = tyś lubił = ty żeś lubił, as you can see there are multiple levels of contraction
  --here and while officially these are called past tense verb forms and taught as such, they have a slightly unusual origin story; all three of
  --these variants are understandable, though increasingly archaic for the latter two, but still in use sometimes), it has much more regular
  --endings, and only three variants (in most cases two are the same) of the so-called l-participle are needed (one for feminine and neuter, one
  --for masculine singular, one for masculine animate plural). For the conjugate verb function, which is the smart verb function to an extent, 
  --aside from the infinitive, one needs the number of the conjugation it takes, as these are not predictable from just the spelling. This does 
  --not also fully account for changes in the root or conjugations where endings can vary. In addition, there is an irregImpVerb function which
  --works almost the same as conjVerb, but requires being fed the imperative for 2nd person singular, since that one can undergo unpredictable
  --internal sound changes even if the verb is otherwise regular.
  Verb : Type = {s : VForm => Str} ;

  mkVerb : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Verb
    = \inf,imp2sg,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3,lpart,lpartmascsg,lpartmascpl -> {
    s = table {
      Inf => inf ; --lubić
      Imp2Sg => imp2sg ; --lub!
      --present
      Pres (NPAgr Sg First) => pressg1 ; --lubię
      Pres (NPAgr Sg Second) => pressg2 ; --lubisz
      Pres (NPAgr Sg Third) => pressg3 ; --lubi
      Pres (NPAgr Pl First) => prespl1 ; --lubimy
      Pres (NPAgr Pl Second) => prespl2 ; --lubicie
      Pres (NPAgr Pl Third) => prespl3 ; --lubią
      --past singular
      Past (NPGAgr Sg First Fem) => lpart + "am" ; --lubiłam
      Past (NPGAgr Sg First Neut) => lpart + "om" ; --lubiłom 
      Past (NPGAgr Sg First Masc) => lpartmascsg + "em" ; --lubiłem
      Past (NPGAgr Sg First MascAnim) => lpartmascsg + "em" ; --lubiłem
      Past (NPGAgr Sg Second Fem) => lpart + "aś" ; --lubiłaś
      Past (NPGAgr Sg Second Neut) => lpart + "oś" ; --lubiłoś
      Past (NPGAgr Sg Second Masc) => lpartmascsg + "eś" ; --lubiłeś
      Past (NPGAgr Sg Second MascAnim) => lpartmascsg + "eś" ; --lubiłeś
      Past (NPGAgr Sg Third Fem) => lpart + "a" ; --lubiła
      Past (NPGAgr Sg Third Neut) => lpart + "o" ; --lubiło
      Past (NPGAgr Sg Third Masc) => lpartmascsg ; --lubił
      Past (NPGAgr Sg Third MascAnim) => lpartmascsg ; --lubił
      --past plural
      Past (NPGAgr Pl First Fem) => lpart + "yśmy" ;--lubiłyśmy
      Past (NPGAgr Pl First Neut) => lpart + "yśmy" ; --lubiłyśmy
      Past (NPGAgr Pl First Masc) => lpart + "yśmy" ; --lubiłyśmy
      Past (NPGAgr Pl First MascAnim) => lpartmascpl + "iśmy" ; --lubiliśmy
      Past (NPGAgr Pl Second Fem) => lpart + "yście" ; --lubiłyście
      Past (NPGAgr Pl Second Neut) => lpart + "yście" ; --lubiłyście
      Past (NPGAgr Pl Second Masc) => lpart + "yście" ; --lubiłyście
      Past (NPGAgr Pl Second MascAnim) => lpartmascpl + "iście" ; --lubiliście
      Past (NPGAgr Pl Third Fem) => lpart + "y" ; --lubiły
      Past (NPGAgr Pl Third Neut) => lpart + "y" ; --lubiły
      Past (NPGAgr Pl Third Masc) => lpart + "y" ; --lubiły
      Past (NPGAgr Pl Third MascAnim) => lpartmascpl + "i"  --lubili
      } 
    } ;

  conjVerb : Str -> Conjugation -> Verb = \inf, conj -> case conj of {
     I => case inf of { czyt  +  "ać" => mkVerb inf (czyt + "aj") (czyt + "am") (czyt + "asz") (czyt + "a") (czyt + "amy") (czyt + "acie") (czyt + "ają") (czyt + "ał") (czyt + "ał") (czyt + "al")} ; -- 1st conjugation
     II => case inf of { umi  +  "eć" =>  mkVerb inf (umi + "ej") (umi + "em") (umi + "esz") (umi + "e") (umi + "emy") (umi + "ecie") (umi + "eją") (umi + "ał") (umi + "ał") (umi + "el") } ;  -- 2nd conjugation
     III => case inf of { tani + "eć"  => mkVerb inf (tani + "ej") (tani + "eję") (tani + "ejesz") (tani + "eje") (tani + "ejemy") (tani + "ejcie") (tani + "eją") (tani + "ał") (tani + "ał") (tani + "el") } ;  -- 3rd conjugation
     IV => case inf of { mal  +  "ować" => mkVerb inf (mal + "uj") (mal + "uję") (mal + "ujesz") (mal + "uje") (mal + "ujemy") (mal + "ujecie") (mal + "ują") (mal + "ował") (mal + "ował") (mal + "owal") } ;  -- 4th conjugation
     Va => case inf of { ciag  +  "nąć" => mkVerb inf (ciag + "nij") (ciag + "nę") (ciag + "niesz") (ciag + "nie") (ciag + "niemy") (ciag + "niecie") (ciag + "ną") (ciag + "nęł") (ciag + "nął") (ciag + "nęl") } ;  -- 5th A conjugation
     Vb => case inf of { ply  +  "nąć" => mkVerb inf (ply + "ń") (ply + "nę") (ply + "niesz") (ply + "nie") (ply + "niemy") (ply + "niecie") (ply + "ną") (ply + "nęł") (ply + "nął") (ply + "nęl") } ;  -- 5th B conjugation
     Vc => case inf of { chud  +  "nąć" => mkVerb inf (chud + "nij") (chud + "nę") (chud + "niesz") (chud + "nie") (chud + "niemy") (chud + "niecie") (chud + "ną") (chud + "ł") (chud + "ł") (chud + "l") } ;  -- 5th C conjugation
     VIa => case inf of { rob  +  "ić" => mkVerb inf rob (rob + "ię") (rob + "isz") (rob + "i") (rob + "imy") (rob + "icie") (rob + "ią") (rob + "ił") (rob + "ił") (rob + "il") } ;  -- 6th A conjugation
     VIb => case inf of { wierz  +  "yć" => mkVerb inf wierz (wierz + "ę") (wierz + "ysz") (wierz + "y") (wierz + "ymy") (wierz + "ycie") (wierz + "ą") (wierz + "ył") (wierz + "ył") (wierz + "yl") } ;  -- 6th B conjugation
     VIIa => case inf of { widz  +  ("ieć"|"eć") => mkVerb inf widz (widz + "ę") (widz + "isz") (widz + "i") (widz + "imy") (widz + "icie") (widz + "ą") (widz + "ał") (widz + "ał") (widz + "el") } ;  -- 7th A conjugation
     VIIb => case inf of { lez  +  ("ieć"|"eć") => mkVerb inf lez (lez + "ę") (lez + "ysz") (lez + "y") (lez + "ymy") (lez + "ycie") (lez + "ą") (lez + "ał") (lez + "ał") (lez + "el") } ;  -- 7th B conjugation
     VIIIa => case inf of { czyt  +  "ywać" => mkVerb inf (czyt + "uj") (czyt + "uję") (czyt + "ujesz") (czyt + "uje") (czyt + "ujemy") (czyt + "ujecie") (czyt + "ują") (czyt + "ywał") (czyt + "ywał") (czyt + "ywal") } ;  -- 8th A conjugation
     VIIIb => case inf of { zysk  +  "iwać" => mkVerb inf (zysk + "uj") (zysk + "uję") (zysk + "ujesz") (zysk + "uje") (zysk + "ujemy") (zysk + "ujecie") (zysk + "ują") (zysk + "iwał") (zysk + "iwał") (zysk + "iwal") } ;  -- 8th B conjugation
     IX => case inf of { lam  +  "ać" => mkVerb inf lam (lam + "ię") (lam + "iesz") (lam + "ie") (lam + "iemy") (lam + "iecie") (lam + "ią") (lam + "ał") (lam + "ał") (lam + "al") } ; -- 9th conjugation
     -- some verbs in the 9th conjugation mutate the root https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_IX and are not accounted for
     Xa => case inf of { pi  +  "ć" => mkVerb inf (pi + "j") (pi + "ję") (pi + "jesz") (pi + "je") (pi + "jemy") (pi + "jecie") (pi + "ją") (pi + "ł") (pi + "ł") (pi + "l") } ; -- 10th A conjugation
     Xb => case inf of { l  +  "ać" => mkVerb inf (l + "ej") (l + "eję") (l + "ejesz") (l + "eje") (l + "ejemy") (l + "ejecie") (l + "eją") (l + "ał") (l + "ał") (l + "al") }  -- 10th B conjugation
     -- 10th C conjugation mutates the root too and has variation in endings https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_Xc
     -- 11th conjugation mutates the root as well https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_XI
     -- certain changes in the imperative, such as internal vowel shifts, are not accounted for (as they are not conjugation-specific)
     } ;

  irregImpVerb : Str -> Str -> Conjugation -> Verb = \inf, imp, conj -> case conj of {
     I => case inf of { czyt  +  "ać" => mkVerb inf imp (czyt + "am") (czyt + "asz") (czyt + "a") (czyt + "amy") (czyt + "acie") (czyt + "ają") (czyt + "ał") (czyt + "ał") (czyt + "al")} ; -- 1st conjugation
     II => case inf of { umi  +  "eć" =>  mkVerb inf imp (umi + "em") (umi + "esz") (umi + "e") (umi + "emy") (umi + "ecie") (umi + "eją") (umi + "ał") (umi + "ał") (umi + "el") } ;  -- 2nd conjugation
     III => case inf of { tani + "eć"  => mkVerb inf imp (tani + "eję") (tani + "ejesz") (tani + "eje") (tani + "ejemy") (tani + "ejcie") (tani + "eją") (tani + "ał") (tani + "ał") (tani + "el") } ;  -- 3rd conjugation
     IV => case inf of { mal  +  "ować" => mkVerb inf imp (mal + "uję") (mal + "ujesz") (mal + "uje") (mal + "ujemy") (mal + "ujecie") (mal + "ują") (mal + "ował") (mal + "ował") (mal + "owal") } ;  -- 4th conjugation
     Va => case inf of { ciag  +  "nąć" => mkVerb inf imp (ciag + "nę") (ciag + "niesz") (ciag + "nie") (ciag + "niemy") (ciag + "niecie") (ciag + "ną") (ciag + "nęł") (ciag + "nął") (ciag + "nęl") } ;  -- 5th A conjugation
     Vb => case inf of { ply  +  "nąć" => mkVerb inf imp (ply + "nę") (ply + "niesz") (ply + "nie") (ply + "niemy") (ply + "niecie") (ply + "ną") (ply + "nęł") (ply + "nął") (ply + "nęl") } ;  -- 5th B conjugation
     Vc => case inf of { chud  +  "nąć" => mkVerb inf imp (chud + "nę") (chud + "niesz") (chud + "nie") (chud + "niemy") (chud + "niecie") (chud + "ną") (chud + "ł") (chud + "ł") (chud + "l") } ;  -- 5th C conjugation
     VIa => case inf of { rob  +  "ić" => mkVerb inf rob imp (rob + "isz") (rob + "i") (rob + "imy") (rob + "icie") (rob + "ią") (rob + "ił") (rob + "ił") (rob + "il") } ;  -- 6th A conjugation
     VIb => case inf of { wierz  +  "yć" => mkVerb inf wierz imp (wierz + "ysz") (wierz + "y") (wierz + "ymy") (wierz + "ycie") (wierz + "ą") (wierz + "ył") (wierz + "ył") (wierz + "yl") } ;  -- 6th B conjugation
     VIIa => case inf of { widz  +  ("ieć"|"eć") => mkVerb inf imp (widz + "ę") (widz + "isz") (widz + "i") (widz + "imy") (widz + "icie") (widz + "ą") (widz + "ał") (widz + "ał") (widz + "el") } ;  -- 7th A conjugation
     VIIb => case inf of { lez  +  ("ieć"|"eć") => mkVerb inf imp (lez + "ę") (lez + "ysz") (lez + "y") (lez + "ymy") (lez + "ycie") (lez + "ą") (lez + "ał") (lez + "ał") (lez + "el") } ;  -- 7th B conjugation
     VIIIa => case inf of { czyt  +  "ywać" => mkVerb inf imp (czyt + "uję") (czyt + "ujesz") (czyt + "uje") (czyt + "ujemy") (czyt + "ujecie") (czyt + "ują") (czyt + "ywał") (czyt + "ywał") (czyt + "ywal") } ;  -- 8th A conjugation
     VIIIb => case inf of { zysk  +  "iwać" => mkVerb inf imp (zysk + "uję") (zysk + "ujesz") (zysk + "uje") (zysk + "ujemy") (zysk + "ujecie") (zysk + "ują") (zysk + "iwał") (zysk + "iwał") (zysk + "iwal") } ;  -- 8th B conjugation
     IX => case inf of { lam  +  "ać" => mkVerb inf imp (lam + "ię") (lam + "iesz") (lam + "ie") (lam + "iemy") (lam + "iecie") (lam + "ią") (lam + "ał") (lam + "ał") (lam + "al") } ; -- 9th conjugation
     Xa => case inf of { pi  +  "ć" => mkVerb inf imp (pi + "ję") (pi + "jesz") (pi + "je") (pi + "jemy") (pi + "jecie") (pi + "ją") (pi + "ł") (pi + "ł") (pi + "l") } ; -- 10th A conjugation
     Xb => case inf of { l  +  "ać" => mkVerb inf imp (l + "eję") (l + "ejesz") (l + "eje") (l + "ejemy") (l + "ejecie") (l + "eją") (l + "ał") (l + "ał") (l + "al") }  -- 10th B conjugation
     } ;
  
  negation : Bool -> Str = \b -> case b of {True => [] ; False => "nie"} ; 

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {cp : Str ; rp : Case ; rn : Case} ; 
  --though almost all the verbs in the lexicon take Acc, this is not always the case, and it can change between positive and negative sentences,
  --and verbs that have obligatory prepositions.

  --Verb2 gives the warning: ignoring lock fields in resolving mkV2, but otherwise works; we were not able to figure out in the lab what causes it.

  be_Verb : Verb = mkVerb "być" "bądź" "jestem" "jesteś" "jest" "jesteście" "jesteśmy" "są" "był" "był" "byl" ; 
  
  --all of the gVerb was removed as it was not something key to the structure of any grammar, but a way of handling how it works in English,
  --and it was not needed in mine.
}