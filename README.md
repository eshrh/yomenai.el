# yomenai.el
Integrated japanese reading environment for emacs. Right now, this is
just a collection of some elisp for learning japanese with immersion.

## SDCV segment search
`sdcv-segment-search.el` provides the function `sdcv-segment-search`
which lets you search for the word under the cursor no matter where
you are on that word. It requires both sdcv-mode which is on MELPA and
[syohex](https://github.com/syohex)'s mecab package. This doesn't seem
to be on any package hosting, but you can make your own recipe for
it. I use straight.el, so it looks like:

```emacs-lisp
(straight-use-package '(mecab :type git
                              :repo "https://github.com/syohex/emacs-mecab"
                              :pre-build ("make")
                              :files ("mecab-core.so"
                                      "mecab-core.o"
                                      "mecab-core.c"
                                      "mecab.el")))
```

Of course, this also requires the actual mecab program and some
dictionary.  Customize the `mecab-dictionary-path` variable to point
to your mecab dictionary folder.  The default points to this location
on Arch Linux after installing `mecab-ipadic` from the AUR.

## Dictionaries 
Use yomi2star.sh to convert yomichan zip files to
stardict format. This depends on 
+ yomi2tab.py written by [olety](https://github.com/olety) which depends on pandas.
+ pyglossary
