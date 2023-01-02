# gimp-extract-text
script-fu to batch extract text info from .xcf files for indexing purposes

Added: a mode with Extra Annotations, which does output a file suitable
for further parsing:

each filename is preceded by
file=

and each text layer with
font=... visible=yes/no x=... y=...:

(so that you can try to sort things if you wish with postprocessing)
