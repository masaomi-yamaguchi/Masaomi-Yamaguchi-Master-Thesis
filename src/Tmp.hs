module Tmp where

-- Fail
-- We may revise this.
data Nat = Z
         | S Nat

-- | Abstract addresses
data Tree a = N a [Tree a] -- polymorphic tree type

data Lab = A [Char] [Char] -- Attribute
         | T [Char] -- Text
         | E [Char] -- Element
         | Na Nat

q6 :: Tree Lab -> Tree Lab
q6 t = let (N (E "book") ts) = t
       in let (ts', n) = q6_section ts
          in N (E "toc") ts'

q6_section :: [Tree Lab] -> ([Tree Lab], Nat)
q6_section l = case l of
  [] -> ([], Z)
  N (E "section") xs:rest
    -> let (title_attr, xs') = q6_find_title_attr xs
           (xs'', n1) = q6_section xs'
       in let (rest', n2) = q6_section rest
          in ( N
                 (E "section")
                 (append xs' (N (E "figcount") [N (Na n1) []]:xs''))
               :rest'
             , n2)
  N (E "figure") fig:rest -> let (rest', n) = q6_section rest
                             in (rest', S n)
  node:rest -> q6_section rest

q6_find_title_attr :: [Tree Lab] -> ([Tree Lab], [Tree Lab])
q6_find_title_attr l = case l of
  [] -> ([], [])
  N (A a b) []:rest -> let (ta, rest') = q6_find_title_attr rest
                       in (N (A a b) ta:rest', rest')
  N (E "title") title:rest -> (N (E "title") title:[], rest)
  node:rest -> (node:rest, [])

append :: [a] -> [a] -> [a]
append l1 l2 = case l1 of
  []   -> l2
  x:xs -> x:append xs l2

s = N
  (E "book")
  [ N (E "title") [N (T "Data on the Web") []]
  , N (E "author") [N (T "Serge Abiteboul") []]
  , N (E "author") [N (T "Peter Buneman") []]
  , N (E "author") [N (T "Dan Suciu") []]
  , N
      (E "section")
      [ N (A "id" "intro") []
      , N (A "difficulty" "easy") []
      , N (E "title") [N (T "Introduction") []]
      , N (E "p") [N (T "Text ... ") []]
      , N
          (E "section")
          [ N (E "title") [N (T "Audience") []]
          , N (E "p") [N (T "Text ... ") []]]
      , N
          (E "section")
          [ N (E "title") [N (T "Web Data and the Two Cultures") []]
          , N (E "p") [N (T "Text ... ") []]
          , N
              (E "figure")
              [ N (A "height" "400") []
              , N (A "width" "400") []
              , N
                  (E "title")
                  [N (T "Traditional client/server architecture") []]
              , N (E "image") [N (A "source" "csarch.gif") []]]
          , N (E "p") [N (T "Text ...") []]]]
  , N
      (E "section")
      [ N (A "id" "syntax") []
      , N (A "difficulty" "medium") []
      , N (E "title") [N (T "A Syntax For Data") []]
      , N (E "p") [N (T "Text ... ") []]
      , N
          (E "figure")
          [ N (A "height" "200") []
          , N (A "width" "500") []
          , N (E "title") [N (T "Graph representations of structures") []]
          , N (E "image") [N (A "source" "graphs.gif") []]]
      , N (E "p") [N (T "Text ... ") []]
      , N
          (E "section")
          [ N (E "title") [N (T "Base Types") []]
          , N (E "p") [N (T "Text ...") []]]
      , N
          (E "section")
          [ N (E "title") [N (T "Representing Relational Databases") []]
          , N (E "p") [N (T "Text") []]
          , N
              (E "figure")
              [ N (A "height" "250") []
              , N (A "width" "400") []
              , N (E "title") [N (T "Examples of Relations") []]
              , N (E "image") [N (A "source" "relatios.gif") []]]]
      , N
          (E "section")
          [ N (E "title") [N (T "Representing Object Databases") []]
          , N (E "p") [N (T "Text ... ") []]]]]
