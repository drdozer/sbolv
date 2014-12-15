
function parsePigeon(input)
{
    var glyphs = {
        '?': 'user-defined',
        '3': '3-prime-overhang',
        '5': '5-prime-overhang',
        'p': 'promoter',
        'P': 'promoter',
        'r': 'res',
        'c': 'cds',
        'g': 'user-defined',
        'f': 'fusion',
        't': 'terminator',
        'T': 'terminator',
        's': 'signature',
        'o': 'operator',
        '>': 'user-defined',
        '<': 'user-defined',
        '|': 'restriction-site',
        'z': 'user-defined',
        'x': 'user-defined',
        'd': 'user-defined',
        'v': 'v',
        '-': 'user-defined',
        '.': 'user-defined'
    };

    var colors =
        ['blue',
         'darkblue',
         'green',
         'darkgreen',
         'red',
         'darkred',
         'orange',
         'darkorange',
         'purple',
         'darkpurple',
         'yellow',
         'darkyellow',
         'gray',
         'black'];

    var output = {
        version: 1,
        segments: [
            { id: 'payload',
              name: 'Payload',
              thickness: 1,
              sequence: []
            }
        ],

        joins: [],
        arcs: []
    };

    var parsingArcs = false;

    var readingColors = 0;

    var lineNum = 0;
       
    input.split('\n').forEach(function(line) {

        ++ lineNum;

        var errorPrefix = 'Line ' + lineNum + ': ';

        line = line.trim();

        if(line.length === 0)
            return;
        
        if(line == 'rgbcolors') {
            readingColors = colors.length;
            return;
        }

        if(readingColors) {
            colors[colors.length - (readingColors --)] = line;
            return;
        }

        if(line == '# Arcs') {
            parsingArcs = true;
            return;
        }

        var tokens = line.split(' ');

        if(parsingArcs) {

            if(tokens.length != 3)
                throw new Error(errorPrefix + 'Expected exactly 3 tokens for arc line');

            var arcSource = tokens[0],
                arcType = ({ ind: 'induction', rep: 'repression' })[tokens[1]],
                arcTarget = tokens[2];

            if(arcType === undefined)
                throw new Error(errorPrefix + 'Unknown arc type: expected `ind` or `rep`');

            output.arcs.push({
                from: arcSource,
                to: arcTarget,
                type: arcType
            });

            return;
        }

        var glyph = {
            direction: 'rightwards'
        };

        var type = tokens[0];

        if(type[0] == '<') {
            glyph.direction = 'leftwards';
            type = type.slice(1);
        } else if(type[0] == '>') {
            type = type.slice(1);
        }

        if((glyph.type = glyphs[type]) === undefined)
            throw new Error(errorPrefix + 'Unknown glyph type');
        
        if(tokens.length >= 2)
            glyph.name = tokens[1];

        if(tokens.length >= 3) {
            if((glyph.color = colors[parseInt(tokens[2]) - 1]) === undefined) {
                throw new Error(errorPrefix + 'Unknown color value');
            }
        }

        if(tokens.length >= 4) {

            if(tokens[3] != 'nl')
                throw new Error(errorPrefix + 'Unknown token; only "nl" is valid here');

            glyph.label = false;
        }

        if(tokens.length >= 5)
            throw new Error(errorPrefix + 'Too many tokens in line');

        if(glyph.type == 'v') {

            output.segments.push({
              id: 'vector',
              name: 'Vector',
              thickness: 1,
              sequence: []
            });

            output.joins.push(
              { from: 'vector', to: 'payload' },
              { from: 'payload', to: 'vector' });

            if(glyph.name !== undefined) {
                output.segments[1].sequence.push({
                    type: 'cds',
                    direction: 'rightwards',
                    name: glyph.name
                });
            }

            if(glyph.color !== undefined)
                output.segments[1].color = glyph.color;
        }

        output.segments[0].sequence.push(glyph);
    });

    if(!parsingArcs)
        throw new Error(errorPrefix + 'No arcs section found');

    return output;
};




