\version "2.19.82"

% \include "oll-core/package.ily"
% \loadPackage notation-fonts

% \displayNotationFonts

#(set-global-staff-size 20)


\paper {
  #(set-paper-size "a2" 'landscape)
  top-margin = 105\mm
  bottom-margin = 105\mm
  left-margin = 147\mm
  right-margin = 147\mm
  indent = 0
  ragged-last-bottom = ##f
}

\header {
  tagline = ##f
}

\include "art_content.ily"