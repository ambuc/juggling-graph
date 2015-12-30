svg = d3.select('.stage')
frame = height = width = null

window.render = (str) ->
	
	frame = svg.attr('class','frame')
	height = frame.style('height').replace('px','')
	width  = frame.style('width' ).replace('px','')

	svg.selectAll("*").remove()

	writeDefs()

	nodes = parseStr(str, 0)
	nodes.pop() #gets rid of last null bit

	#compile list of senders and receivers
	senders = []
	senders.push( index ) for node, index in nodes when node['throw']
	recievers = []
	recievers.push( index ) for node, index in nodes when node['catch']

	len = str.length

	drawRect( index, len) for node, index in nodes when node['inPlex'] is true

	writeChar( index, char, len) for char, index in str
	
	calculateArrow(sender, nodes, senders, recievers, len) for sender in senders

	null

writeDefs = () ->
	svg.append('defs').append('marker')
		.attr({
			'markerHeight'	: 5, 'markerWidth'	: 5,
			'refX'			: 0, 'refY'			: 0,
			'viewBox'		: '-5 -5 10 10',
			'id'			: 'marker_arrow',
			'markerUnits'	: 'strokeWidth',
			'orient'		: 'auto'
		})
	.append('path')
		.attr({
			'd': "M 0,0 m -5,-5 L 5,0 L -5,5 Z",
			'fill': 'darkgrey'
		})
	null


#recursive front-first parsing function, which creates an array of javascript objects containing useful semantic information about eech node's role and function. 
#sadly, returns an array of the form [foo, ..., bar, undefined], and so must be trimmed with .pop() after
parseStr = (str, index, inUnit = false, currUnit = 0) ->
	if (str == "")
		return
	else
		char = str[0]
		node =
			character: char
			int: if char not in ['[',']'] then parseInt(char, 36)
			index: index
			throw: if char not in ['[',']'] then true else false
			catch: if not inUnit or char is '[' then true else false
			inPlex: if ((char in ['[',']']) or inUnit) then true else false
			inUnit: inUnit
			unit: currUnit
		switch char
			when '[' then inUnit = true
			when ']' then inUnit = false
			else inUnit = inUnit
		currUnit = if inUnit then currUnit else currUnit+1
		return [node].concat( parseStr( str[1..], index+1, inUnit, currUnit ) )

#draws a rectangle on the canvas, given index and string length (subdivisions)
drawRect = (i, len) ->
	svg.append('rect')
		.attr({
			'x': width / (len+1) * (i+0.5) ,
			'y': height / 2 - 20,
			'width': width / (len+1) * 1.01,
			'height': 40,
			'fill':'lightblue'
		})

#writes a character on the canvas
writeChar = (i, char, len) ->
	svg.append('text').text( char )
		.style('text-anchor','middle')
		.style('alignment-baseline','middle')
		.attr({
			'x': width / (len + 1) * (i+1),
			'y': height / 2.0
		})
	null

#calculates the endpoints and height of an arrow from any given sender node
calculateArrow = (sender, nodes, senders, recievers, len) ->
	ob_i			= nodes[sender] #this node in the $nodes array
	left_index		= ob_i['index']
	larger_unit		= if ob_i['inPlex'] then ob_i['unit'] else left_index
	displacement	= ob_i['int']
	unwrapped_diff	= recievers.indexOf( larger_unit ) + displacement
	wrapped_diff	= unwrapped_diff  %  recievers.length
	right_index		= recievers[wrapped_diff]
	left_x			= width / (len+1) * ( left_index + 1)
	right_x			= width / (len+1) * (right_index + 1)
	pad				= 10
	bMod			= if (left_x > right_x) then 1 else -1
	y				= (height / 2.0) + (pad * bMod)
	
	drawArrow(svg,left_x,right_x,y)

	null

#draws an arrow between two nodes
drawArrow = (svg, x1, x2, y) ->
	r = 0.5*Math.abs(x2 - x1)
	curvestring = "M#{x1},#{y} A #{r},#{r} 0 0,1 #{x2},#{y}"
	svg.append("path")
		.attr({
			'd':curvestring,
			'stroke':'grey',
			'stroke-width':2,
			'fill':'none',
			'stroke-linecap':'round',
			'marker-end':'url(#marker_arrow)',
			'class':'link'
		})

	null

