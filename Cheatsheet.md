# Lambda Terminal Cheatsheet

## General
`i`, `j`, `k`, `l`, arrow keys - move selection  
`Enter` - go to definition  
`g` - go back in location history  
`G` - go forward in location history  
`o` - open new expression definition  
`O` - open new type definition  
`q` - quit  

## Definition view
`Tab` - change wrapping style  
`n` - (re)name selection  
`N` - (re)name definition  
`e` - edit selection  
`)` - call selection with an argument  
`(` - apply a function to selection  
`d` - delete selection  
`c` - copy  
`p` - paste  
`u` - undo  
`r` - redo  

### Specific to expression definitions
`λ`, `\` - wrap selection in a function  
`|` - add alternative to selected funtion  

### Specific to type definitions
`a` - add data constructor below selection  
`A` - add data constructor above selection  
`>` - add parameter after selection  
`<` - add parameter before selection  

## Editing mode (when the cursor is visible)
`Esc` - cancel editing  
`Tab` - commit autocomplete selection  
`Enter` - commit editor content  
`λ`, `\` - insert function (when the editor is empty)  
