### General
- How do i specify that something must be a start of a line?
    - Sørg for at have en regel der holder styr på newline

### Text
- Noget text (fx i *ATX headings*) kan være en tom streng, mens andet(fx i *setext* ikke kan) ikke kan.
- Noget text kan gå over flere linjer, fx i *setext* headings
    - Kan måske løses med en *multiLineText* kategori

### Thematic Break
- a setext heading takes precedence over Thematic Break

### ATX headings
- Hvordan sikrer jeg at det er i starten af en linje?
- "#" kan escapes af "\" hvordan kan jeg representere det?
- En header kan være "tom" for text

### Indented code-block
- if inside a list, text can be indented 4 spaces, without being a code-block

### Fenced code-block
- matching the opening and closing does not seem to be done optimally
- QUESTION: Can i say that i have *some text* and then a *closing fence*? or do i then have to specify that the *some text* part is text that does not look like the *closing fence*.
- How do i specify that it can be terminated by the end of the string?
- QUESTION: How do i specify in a grammar that the *closing fence* must be at least as long as the *opening fence*?  
- QUESTION: How do i specify that if *opening fence* has **X** leading spaces, the first **X** leading spaces of each line in the code should be ignored?
    - Det bliver behandlet efter

### HTML Blocks 
- QUESTION: How do i handle case-insensitive? It seems ineffecient to do in the CFG
- Parsing it to Elm-HTML nodes will require me to parse the entire HTML block structure.
- CommonMark Improvement: HTML Blocks case 1, can be enden by another end tag, than the start tag, leading to weird html output.
- Excluding `script`, `style` and `pre` in case 7, inside tags, but allowing all other words.