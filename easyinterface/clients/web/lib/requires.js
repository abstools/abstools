
	alert("hi");
	//jquery stuff
	$.get("ei/ext/jquery-ui/js/jquery-ui.js");
	//codemirror stuff
	$.get("ei/lib/codemirror/lib/codemirror.js");
	//jstree stuff
	$.get("ei/lib/jstree/jquery.jstree.js");
	$.get("ei/lib/jstree/_docs/syntax/!script.js");
	$.get("ei/lib/jstree/_lib/jquery.hotkeys.js");
	//jquery.svg
	$.get("ei/lib/jquery.svg/jquery.svg.js"); 
	//the codearea stuff
  $.get("ei/js/constants.js");
  $.get("ei/js/set.js");
  $.get("ei/js/easyinterface.js");
  $.get("ei/js/codearea.js");
  $.get("ei/js/filemanager.js");
  $.get("ei/js/console.js");
  $.get("ei/js/checkboxwidget.js");
  $.get("ei/js/combowidget.js");
  $.get("ei/js/parameters.js");
  $.get("ei/js/tools.js");
  $.get("ei/js/toolselectorwidget.js");
  $.get("ei/js/outline.js");
  $.get("ei/js/cmdengine.js");
  $.get("ei/js/outputmanager.js");

  $.get("ei/js/consolecommand.js");
  $.get("ei/js/markerwidget.js");
  $.get("ei/js/inlinedmarkerwidget.js");
  $.get("ei/js/addmarkercommand.js");
  $.get("ei/js/highlightlinewidget.js");
  $.get("ei/js/highlightlinecommand.js");
  $.get("ei/js/changecsscommand.js");
  $.get("ei/js/onclickaction.js");

  $.get("ei/js/codelineaction.js");

     $( function() {
	var x = new EasyInterface({ 
	    holder: $("#ei_1"),
	    theme: "ei/html/eitheme.resizable.html",
	    cfgFile: "../config/settings.cfg"
	});
    });


//CSS
$('<link/>', {
   rel: 'stylesheet',
   type: 'text/css',
   href: 'ei/ext/jquery-ui/css/redmond/jquery-ui.css'
}).appendTo('head');


