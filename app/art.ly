\version "2.19.82"

% \include "oll-core/package.ily"
% \loadPackage notation-fonts

% \displayNotationFonts

#(set-global-staff-size 16)


\paper {
  #(set-paper-size "a4" 'landscape)
  top-margin = 2.5\cm
  bottom-margin = 2.5\cm
  left-margin = 2.5\cm
  right-margin = 2.5\cm
  indent = 0
  ragged-last-bottom = ##f
}

\header {
  tagline = ##f
}

\include "art_content.ily"