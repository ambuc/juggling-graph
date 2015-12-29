# juggling-graph

This is a visualization engine for [Siteswaps](https://en.wikipedia.org/wiki/Siteswap), a notation system used in juggling to represent patterns in space. It accepts input in the form of numbers `0-9`, letters `a-z`, and brackets `[`,`]`, which denote multiplexes, i.e. synchronous events. `juggling-graph` draws arrows from each valid throwable position to each valid catch position. Multiplexes throw from their contents, but recieve at their opening bracket.

##Example
Input: `39[47]51`
Output: [Image](image)

##Dependencies
Requires [jQuery](https://jquery.com/) and [d3.js](d3js.org).
```
	<script type="text/javascript" src="https://code.jquery.com/jquery-2.1.4.min.js"></script>
	<script type="text/javascript" src="https://d3js.org/d3.v3.min.js"></script>
```

##Usage
A minimal working example is presented below.
```
	<input id="sequence" type="text">
	<svg class='stage'></svg>

	<script type="text/javascript">
		$('#sequence').keyup(function(){
			var str = $('#sequence').val();
			render( str );
		});
	</script>
```
The `render()` function takes a single argument, a string to be parsed. This string is usually a sequence of numbers, but can accept letter input, as parsed in base 36. It also accepts brackets. Sites within brackets are treated as a single synchornous event, and share a catch point.

##Todo
 - draw little colored boxes around multiplexes
 - better arrows / arrow handling w.r.t the aforementioned little colored boxes
 - arrows that change color when hovered over?
 - inherent validation engine
 - better self-loops
