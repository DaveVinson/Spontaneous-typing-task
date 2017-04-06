

// http://www.sitepoint.com/jqueryhtml5-input-focus-cursor-positions/
$.fn.setCursorPosition = function(pos) {
	this.each(function(index, elem) {
	if (elem.setSelectionRange) {
		elem.setSelectionRange(pos, pos);
	} else if (elem.createTextRange) {
		var range = elem.createTextRange();
		range.collapse(true);
		range.moveEnd('character', pos);
		range.moveStart('character', pos);
		range.select();
	}
	});
	return this;
};

//http://stackoverflow.com/questions/4253367/how-to-escape-a-json-string-containing-newline-characters-using-javascript
String.prototype.escapeSpecialChars = function() {
    return this.replace(/\\n/g, "\\n")
               .replace(/\\'/g, "\\'")
               .replace(/\\"/g, '\\"')
               .replace(/\\&/g, "\\&")
               .replace(/\\r/g, "\\r")
               .replace(/\\t/g, "\\t")
               .replace(/\\b/g, "\\b")
               .replace(/\\f/g, "\\f");
};

var keysClean = [];
var rtsClean = [];
var charsClean = [];

var keysAll = [];
var rtsAll = [];
var charsAll = [];

function makeLogBox(ID) {
	document.getElementById(ID).addEventListener("keypress",function(e) {
		logKey(e);
	});
	document.getElementById(ID).addEventListener("keydown",function(e) {
		keyDownProc(e);
	});
	document.getElementById(ID).addEventListener("select",function(e) {
		window.getSelection().empty();
		$('#'+ID).setCursorPosition(document.getElementById(ID).value.length);
	});
	document.getElementById(ID).addEventListener("click",function(e) {
		window.getSelection().empty();
		$('#'+ID).setCursorPosition(document.getElementById(ID).value.length);
	});
}

function logKey(event) {
	rtsClean.push(performance.now());
	charsClean.push(event.charCode);
	keysClean.push(event.keyCode);

	rtsAll.push(performance.now());
	charsAll.push(event.charCode);
	keysAll.push(event.keyCode);
	if (printClean) { console.log(keysClean + '<br />' + rtsClean + '<br />' + charsClean); }
}

function blinkScreen() {
	$("html, body").animate({ opacity: 0.25 }, 750).animate({ opacity: 1.0 }, 750);
}

function keyDownProc(event) {
	theKey = event.keyCode;
	if (theKey == 8 & theKey != 16) {
		rtsAll.push(performance.now());
		charsAll.push(8);
		keysAll.push(event.keyCode);
		rtsClean.splice(rtsClean.length-1,2);
		keysClean.splice(keysClean.length-1,2);
		charsClean.splice(charsClean.length-1,2);
		if (printClean) { console.log(keysClean + '<br />' + rtsClean + '<br />' + charsClean); }
	}
}

function mouseCheck(event) {
	backTry = backTry + 1;
	document.getElementById('mouseorbackspace').value = backTry;
}

function saveData() {
	var dataBlob = '{ "sonaid":"'+$('#sonaid').val()+'",'
		+'"UTC":"'+moment().format('MMMM Do YYYY, h:mm:ss a')+'",'
		+'"title":"'+$('#title').val()+'",'
		+'"genre":"'+$('#genre').val()+'",'
		+'"rtsClean":"'+JSON.stringify(rtsClean)+'",'
		+'"charsClean":"'+JSON.stringify(charsClean)+'",'
		+'"keysClean":"'+JSON.stringify(keysClean)+'",'
		+'"rtsAll":"'+JSON.stringify(rtsAll)+'",'
		+'"charsAll":"'+JSON.stringify(charsAll)+'",'
		+'"keysAll":"'+JSON.stringify(keysAll)+'",'
		+'"content":'+JSON.stringify($('#content').val()).escapeSpecialChars()
		+' }';
	return(dataBlob);
}

function sendData(dataBlob) {
	var ajaxReq = new XMLHttpRequest();
	var params = "dataBlob="+dataBlob;
    ajaxReq.open("POST", "saveTyping.php", true); 
	ajaxReq.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    ajaxReq.send(params); 
}

// http://momentjs.com/


