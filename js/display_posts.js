(function display_posts() {
    document.body.appendChild(document.createElement("br"));
    var div = document.createElement("div");
    document.body.appendChild(div);

    var start = input_text_maker("start", div);
    start.value = "0";
    div.appendChild(document.createElement("br"));

    var many = input_text_maker("many", div);
    many.value = "100";
    div.appendChild(document.createElement("br"));

    var button = document.createElement("input");
    button.type = "button";
    button.value = "display posts";
    button.onclick = doit;

    var show_here = document.createElement("div");


    div.appendChild(button);
    div.appendChild(document.createElement("br"));
    div.appendChild(show_here);

    function doit(){
	//working here.
	var s = parseInt(start.value);
	var m = parseInt(many.value);
	var foo = "";
	show_here.innerHTML = "";
	variable_public_get(["lookup", s, m], function(l) {
	    console.log(JSON.stringify(l));
	    for (i = 1; i< l.length; i++) {
		var p = document.createElement("p");
		console.log(JSON.stringify(l[i]));
		p.innerHTML = atob(l[i]);
		show_here.appendChild(p);
		var divider = document.createElement("p");
		divider.innerHTML = "<font color=\"red\"> =======================</font>";
		show_here.appendChild(divider);
	    }
	});
    }
})();
