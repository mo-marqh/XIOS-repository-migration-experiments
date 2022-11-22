
Plotly.d3.select('body')
    .append('div')
    .html("Select a CSV XIOS file");

// ------------------ select csv data
var input = Plotly.d3.select("body").append("input")
    .attr("type","file")
    .attr("accept",".csv")
    .on("change", handleFileSelect)


// Single file handler
function handleFileSelect() {
    // Check for the various File API support.
    if (window.File && window.FileReader && window.FileList && window.Blob) {
    // Great success! All the File APIs are supported.
    } else {
        alert('The File APIs are not fully supported in this browser.');
    }

    var f = event.target.files[0]; // FileList object
    var reader = new FileReader();

    reader.onload = function(event) {
        makeplot_read_selected(event.target.result)
    };
    // Read in the file as a data URL.
    reader.readAsDataURL(f);
}


function makeplot_read_selected(filename) {
    Plotly.d3.csv(filename,  function(d) {
	// formating data
 	return {
 	    time: +d.time,
 	    event: d.event,
 	    vsize: +d.vsize,
 	    rss: +d.rss,
 	    VmHWM: +d.VmHWM
 	};
    }, function(xios_mem_obj) {
 	processData(xios_mem_obj)
    });
};

function processData(xios_mem_data) {

    // transpose data to use it more easily
    var transData = [];
    for(var i = 0; i < 5 ; i++){
	transData.push([]);
    };
    for(var i = 0; i < xios_mem_data.length ; i++){
	//console.log( xios_mem_data[i]  );
	transData[0].push( xios_mem_data[i].time )   // x axis
	transData[1].push( xios_mem_data[i].vsize ) // y axis
	transData[2].push( xios_mem_data[i].event )  // labels
	transData[3].push( xios_mem_data[i].rss ) // y axis
	transData[4].push( xios_mem_data[i].VmHWM ) // y axis
    };
    //console.log(transData[0]);

    var data = [];
    var result = {
	x: transData[0],
	y: transData[1],
	type: 'scatter',
	mode: 'markers',
	text: transData[2],
        name:'vsize',
    };
    data.push(result);
    
    var result2 = {
        x: transData[0],
        y: transData[3],
        type: 'scatter',
        mode: 'markers',
        text: transData[2],
        name:'RSS',
    };
    data.push(result2);
    
    var result3 = {
        x: transData[0],
        y: transData[4],
        type: 'scatter',
        mode: 'markers',
        text: transData[2],
        name:'VmHWM',
    };
    data.push(result3);

    var layout = {
	title: "Memory consumption",
	xaxis: {
	    title: "Time (s)",
	},
	yaxis: {
	    title: "Memory (Mo)",
	}
    };

    Plotly.newPlot('myDiv', data, layout);
    
}
