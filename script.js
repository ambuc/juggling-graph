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

	var obj = [];

	var inUnit = false;
	var currUnit = 0;
	
	//Assemble OBJ thru STR
	for(var i = 0; i < len; i++){
		if(str[i]==']'){
			inUnit = false;
			obj.push({
				'char' : ']', 'int' : null, 'index' : i,
				'throw' : false, 'catch' : false, 'unit' : currUnit
			});
			currUnit++;
			continue;
		} else if (str[i] == '['){
			inUnit = true;
			currUnit = i;
			obj.push({
				'char' : '[', 'int'  : null, 'index': i,
				'throw': false, 'catch': true, 'unit' : currUnit
			});
			continue;
		} else { //is just a regular number, seems like
			if(inUnit){
				obj.push({
					'char' : str[i], 'int' : parseInt(str[i],36), 'index' : i,
					'throw' : true, 'catch' : false, 'unit' : currUnit
				});
				continue;
			} else {
				obj.push({
					'char' : str[i], 'int' : parseInt(str[i],36), 'index' : i,
					'throw' : true, 'catch' : true, 'unit' : currUnit
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

	for(var i = 0; i < senders.length; i++){
		var interval = obj[i]['int'];
		var left_index = obj[senders[i]]['index'];
		var right_index = recievers[(recievers.indexOf(obj[senders[i]]['unit'])+obj[senders[i]]['int'] )%(recievers.length)]
		
		var left_x  = width / (len+1) * (left_index  +1);
		var right_x = width / (len+1) * (right_index +1);

		var pad = 10;
		var bMod = (left_x>right_x)?1:-1;
		var y = (height / 2.0) + (pad * bMod);

		drawArrow(svg,left_x,right_x,y);
	}
}

var drawArrow = function(svg, left_x, right_x, y){

	var pm = (left_x < right_x)?-1:1;

	var a_x = left_x - pm*3;	var a_y = y + pm*7.5;
	var d_x = right_x;          var d_y = y;
	var b_x = a_x;		var b_y = a_y + pm*Math.sqrt(Math.abs(a_x - d_x))*8;
	var c_x = d_x;		var c_y = b_y;

	var curvestring = "M"+a_x+","+a_y+" C"+b_x+","+b_y+" "+c_x+","+c_y+" "+d_x+","+d_y;

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
