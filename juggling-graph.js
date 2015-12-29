//arrows from http://bl.ocks.org/dustinlarimer/5888271

var svg = d3.select('.stage');

var render = function(str){

	svg.selectAll("*").remove();

	svg.append('defs').append('marker')
		.attr({
			'id':'marker_arrow',
			'markerHeight': 5,
			'markerWidth' : 5,
			'markerUnits' : 'strokeWidth',
			'orient'      : 'auto',
			'refX'        : 0,
			'refY'        : 0,
			'viewBox'     : '-5 -5 10 10'
		})
	.append('path')
		.attr({
			'd': "M 0,0 m -5,-5 L 5,0 L -5,5 Z",
			'fill': 'darkgrey'
		});
	
	var frame = svg.attr('class','frame');
	var height = frame.style('height').replace('px','');
	var width  = frame.style('width' ).replace('px','');

	var charheight = 20;
	var charwidth = 10;

	var len = str.length;

	var obj = [];

	var inUnit = false;
	var currUnit = 0;
	
	//Assemble OBJ thru STR
	for(var i = 0; i < len; i++){
		if(str[i]==']'){
			inUnit = false;
			obj.push({
				'char' : ']', 'int' : null, 'index' : i,
				'throw' : false, 'catch' : false, 'unit' : currUnit,
				'inPlex' : true
			});
			currUnit++;
			continue;
		} else if (str[i] == '['){
			inUnit = true;
			currUnit = i;
			obj.push({
				'char' : '[', 'int'  : null, 'index': i,
				'throw': false, 'catch': true, 'unit' : currUnit,
				'inPlex' : true
			});
			continue;
		} else { //is just a regular number, seems like
			if(inUnit){
				obj.push({
					'char' : str[i], 'int' : parseInt(str[i],36), 'index' : i,
					'throw' : true, 'catch' : false, 'unit' : currUnit,
					'inPlex' : true
				});
				continue;
			} else {
				obj.push({
					'char' : str[i], 'int' : parseInt(str[i],36), 'index' : i,
					'throw' : true, 'catch' : true, 'unit' : currUnit,
					'inPlex' : false
				});
				currUnit++;
				continue;
			}
		}
	}

	var senders = [];
	var recievers = [];
	//compile list of senders and receivers
	for(var i = 0; i < obj.length; i++){
		if ( obj[i]['throw'] ){ senders.push(i);   }
		if ( obj[i]['catch'] ){ recievers.push(i); }
	}

	console.log(obj);
	console.log(senders);
	console.log(recievers);



	//print all boxes
	for(var i = 0; i < obj.length; i++){
		if(obj[i]['inPlex']){
			svg.append('rect')
				.attr({
					'x': width / (len+1) * (i+0.5) ,
					'y': height / 2 - 20,
					'width': width / (len+1) * 1.01,
					'height': 40,
					'fill':'lightblue'
				});
		}
	}


	//write spaced text onscreen
	for(var i = 0; i < len; i++){
		svg.append('text').text(str[i])
			.style('text-anchor','middle')
			.style('alignment-baseline','middle')
			.attr({
				'x': width / (len + 1) * (i+1),
				'y': height / 2.0 
			});
	}

	//print all arrows
	for(var i = 0; i < senders.length; i++){
		var ob_i			= obj[senders[i]]; //this node in the $obj array
		var left_index		= ob_i['index'];
		var larger_unit		= ob_i['inPlex'] ? ob_i['unit'] : left_index;
		var displacement	= ob_i['int']
		var unwrapped_diff	= recievers.indexOf( larger_unit ) + displacement 
		var wrapped_diff	= unwrapped_diff  %  recievers.length ;
		var right_index		= recievers[wrapped_diff];
		
		var left_x  = width / (len+1) * (left_index  +1);
		var right_x = width / (len+1) * (right_index +1);

		var pad = 10;
		var bMod = (left_x>right_x)?1:-1;
		var y = (height / 2.0) + (pad * bMod);

		drawArrow(svg,left_x,right_x,y);
	}
}

var drawArrow = function(svg, x1, x2, y){

	var rx = ry = 0.5*Math.abs(x2 - x1);
	var curvestring = "M"+x1+","+y+" A "+rx+","+ry+" 0 0,1 "+x2+","+y;

	svg.append("path")
		.attr({
			'd':curvestring,
			'stroke':'grey',
			'stroke-width':2,
			'fill':'none',
			'stroke-linecap':'round',
			'marker-end':'url(#marker_arrow)',
			'class':'link'
		});
}
