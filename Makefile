# DO WHAT THE FUCK YOU WANT WITH THE FOLLOWING: (under WTFPL)
I = 2

watch: sheets.elm
	watch -n $(I) elm make sheets.elm --output main.js

reactor:
	elm reactor

