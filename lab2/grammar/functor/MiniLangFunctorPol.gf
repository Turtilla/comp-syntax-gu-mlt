--# -path=.:../abstract

concrete MiniLangFunctorPol of MiniLang = MiniLangFunctor with
  (Grammar = GrammarPol),
  (Syntax = SyntaxPol),
  (Lexicon = LexiconPol)
  ;
  