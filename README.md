# figma-to-pdf

## Instructions

Have a figma file with frames in the top level:     
![alt text](./images/figma-frames.png "Logo Title Text 1")

Get your Figma token from [here](https://www.figma.com/developers)
Make an environment variable e.g. `$ export FIGMA_TOKEN="XXXX-XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"`

```
$ stack build
$ stack exec figma-to-pdf-exe "FILEID"
```

The .pdf will be in the export folder