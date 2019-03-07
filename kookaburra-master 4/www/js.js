document.title = 'ParticipACTION';

//MAKE OVERLAY DISAPPEAR WHEN READY
var overlayChecker = setInterval(function(){
	try{
	  if(!!document.querySelector('table').querySelectorAll('tr').length){
		//console.log('FUCK THE OVERLAY, BABY');
		document.querySelectorAll('.input-daterange input')[0].disabled = true;
		document.querySelectorAll('.input-daterange input')[1].disabled = true;
		overlayFader();
		clearInterval(overlayChecker);
	  }
	}catch(e){}
},250)

function overlayFader(){
	var opacitynow = 1;
	var fader = setInterval(function(){
	  if(opacitynow <= 0){
	  	document.querySelector('.loading-overlay').style.display = 'none';
	  	clearInterval(fader);
	  }
	  document.querySelector('.loading-overlay').style.opacity = opacitynow;
	  opacitynow -= 0.02;
	},10)
}

setInterval(function(){
  try{document.querySelector('body').removeEventListener("click", myInteractionFunc, false);}catch(err){};
  document.querySelector('body').addEventListener("click", myInteractionFunc, false);
},500);
var myInteractionFunc = function(e){
    for (var target=e.target; target && target!=this; target=target.parentNode){
    	console.log(target);
        if(target.matches('.selectize-dropdown')){
        	console.log('SELECTED!');
        }
    }
}

Shiny.addCustomMessageHandler('updatePeriod',shinyResponse);

function shinyResponse(p){
	var dState = p=='Custom'?false:true;
	document.querySelectorAll('.input-daterange input')[0].disabled = dState;
	document.querySelectorAll('.input-daterange input')[1].disabled = dState;	
}

document.addEventListener('scroll', myScrollFunc, false);
function myScrollFunc(){
	//console.log('VINO TINTO!');
	if(window.pageYOffset > 175){
		console.log('SCROLL');
		document.querySelector('.chart_holder').className = 'chart_holder holder_fixed';
	}
	else{
		document.querySelector('.chart_holder').className = 'chart_holder';
	}
	//else{
	//	document.querySelector('.mySortHeaderSet').style.position = 'absolute';
	//	document.querySelector('.mySortHeaderSet').style.top = '0px';
	//	document.querySelector('.mySortHeaderSet').style.zIndex = 1000000;
	//	document.querySelector('.mySortHeaderSet').className = document.querySelector('.mySortHeaderSet').className.split(' fixed')[0];
	//}
	//var dims_col = document.querySelectorAll('.drilldown_true.dm tbody tr td:nth-child(3)');
}

/*
//GO AS FAR BACK AS WE NEED
function ancestorElement(t,n){
	for(i=0;i<n;i++){
		try{t = t.parentElement;}catch(err){}
	}
	return t;
}

//HIDE OR SHOW COLUMNS AS NECESSARY;
function columnNew(n,v){
	var s = v=='Show'?null:'none';
    var t = document.querySelectorAll('table');
    for(i=0;i<t.length;i++){
        var h = t[i].querySelectorAll('th');
        for(j=0;j<h.length;j++){
            if(h[j].innerText == n){
                h[j].style.display = s;
                var r = t[i].querySelectorAll('tbody tr');
                for(k=0;k<r.length;k++){
                    r[k].querySelectorAll('td')[j].style.display = s;
                }
            }
        }
    }
}*/

//TIMEZONE GENERATOR - NOT NEEDED FOR NOW
/*$(document).ready(storeTimeVals = setTimeout(
	function(){
	try{
		Shiny.unbindAll();
		var ict = document.createElement('input');
		ict.id = 'client_time';
		ict.name = 'client_time';
		ict.value = new Date().getTime();
		ict.style.display = 'none';
		var ictz = document.createElement('input');
		ictz.id = 'client_time_zone_offset';
		ictz.name = 'client_time_zone_offset';
		ictz.value = new Date().getTimezoneOffset();
		ictz.style.display = 'none';
		document.body.appendChild(ict);
		document.body.appendChild(ictz);	
		console.log(ict.value);
		console.log(ictz.value);
		Shiny.bindAll();
		clearInterval(storeTimeVals);
	}catch(e){console.log('FAILED');}
},5000))*/

//PASS DRILLDOWN BUTTON INDICES TO DATA LAYER

/*
function getIndices(){
	$("th").off("click");
	var myIndices = setInterval(function(){
		//try{
			dataLayer[0]['indices'] = [];
			for(l=0;l<3;l++){dataLayer[0]['indices'].push([]);} //replace 3 with dynamic val later from Shiny
			var t = document.querySelectorAll('table')[0];
		    //for(i=0;i<t.length;i++){
		        var r = t.querySelectorAll('tbody tr'); //t[i].querySelectorAll('tbody tr');
		        for(k=0;k<r.length;k++){
					var ind = parseInt(r[k].querySelectorAll('td')[0].innerText)-1;
					//console.log(ind);
					for(c=ind;c<dataLayer[0]['indices'].length;c++){
						dataLayer[0]['indices'][c].push(k);
					}
				}
		    //}
		    if(!!dataLayer[0]['indices'][0].length){
		    	setStyles(dataLayer[0]['indices']);
		    	clearInterval(myIndices);
		    }
		//}catch(e){}
	},250)
}
setInterval(function(){getIndices();},750);

function setStyles(t){
	var dt_css = document.head.querySelectorAll('link[href*="css/jquery.dataTables"]');
	dt_css.forEach(function(e) {
	  e.parentElement.removeChild(e);
	});
}

//CLICK CONTROLLER, SAME OLD SAME OLD....
setInterval(function(){
  try{document.querySelector('body').removeEventListener("click", myInteractionFunc, false);}catch(err){};
  document.querySelector('body').addEventListener("click", myInteractionFunc, false);
},500);
var myInteractionFunc = function(e){
    for (var target=e.target; target && target!=this; target=target.parentNode){
    	console.log(target);
        if(target.matches('.tbutton')){
          	var rows = document.querySelectorAll('table tbody')[0].querySelectorAll('tr'); //target.parentElement.parentElement.parentElement.
          	var rows2= document.querySelectorAll('table tbody')[1].querySelectorAll('tr');
          	var rowcount = rows.length;
          	breakloop:
			for(i=0;i<rowcount;i++){
				if(rows[i] == target.parentElement.parentElement){
					for(j=0;j<dataLayer[0]['indices'].length;j++){
						for(k=0;k<dataLayer[0]['indices'][j].length;k++){
							if(dataLayer[0]['indices'][j][k] == i){
								//console.log([i,j,k]);
								var rowA = dataLayer[0]['indices'][j][k]+1;
								var rowB = k==dataLayer[0]['indices'][j].length-1?(rowcount-1):(dataLayer[0]['indices'][j][k+1]-1);
								console.log([rowA,rowB]);
								var hiderows=[];for(s=rowA;s<=rowB;s++){hiderows.push(s);}
								console.log(hiderows);
								var hiddenRows = hiderows.map(r => {
									rows[r].style.display=/expanded/.test(target.className)?'none':null;
									rows2[r].style.display=/expanded/.test(target.className)?'none':null;
								});
								target.className=/expanded/.test(target.className)?'tbutton shrunk':'tbutton expanded';
								target.innerText=/expanded/.test(target.className)?'-':'+';
								break breakloop;
							}
						}
					}
				}
			}
			break;
        }
        else if(target.matches('.navbar-nav a')){
			getIndices();
	        if(/summary/i.test(target.dataset.value)){
	        	for(i=0;i<document.querySelectorAll('.filter-group2').length;i++){document.querySelectorAll('.filter-group2')[i].style.display = 'none';}
	        	for(i=0;i<document.querySelectorAll('.filter-group1').length;i++){document.querySelectorAll('.filter-group1')[i].style.display = 'block';}
	        }
	        else if(/customer/i.test(target.dataset.value)){
	         	for(i=0;i<document.querySelectorAll('.filter-group1').length;i++){document.querySelectorAll('.filter-group1')[i].style.display = 'none';}
	        	for(i=0;i<document.querySelectorAll('.filter-group2').length;i++){document.querySelectorAll('.filter-group2')[i].style.display = 'block';}
	        }
	        else if(/awards.*rewards/i.test(target.dataset.value)){
	         	for(i=0;i<document.querySelectorAll('.filter-group1').length;i++){document.querySelectorAll('.filter-group1')[i].style.display = 'none';}
	        	for(i=0;i<document.querySelectorAll('.filter-group2').length;i++){document.querySelectorAll('.filter-group2')[i].style.display = 'none';}
	        }
			break;
        }
        else if(target.matches('th')){
        	console.log('MAGIC!!!');
        	var tid = ancestorElement(target,6).querySelector('.shiny-bound-output').id;

			for (i=0,len=target.parentElement.children.length;i<len;i++)
			{

			    //(function(index){
			    //    target.parentElement.children[i].onclick = function(){
			    //          alert(index)  ;
			    //    }    
			    //})(i);
			    if(target == target.parentElement.children[i]){
			    	var ix = i;break;
			    }

			}




        	sortActions(target,ix);break;
        }
    }
}

setInterval(function(){
  try{document.querySelectorAll('.dataTables_wrapper')[1].removeEventListener("scroll", myTableSideScrollFunc, false);}catch(err){};
  try{document.querySelectorAll('.dataTables_wrapper')[1].addEventListener("scroll", myTableSideScrollFunc, false);}catch(err){};
  try{document.querySelectorAll('.dataTables_wrapper')[3].removeEventListener("scroll", myTableSideScrollFunc, false);}catch(err){};
  try{document.querySelectorAll('.dataTables_wrapper')[3].addEventListener("scroll", myTableSideScrollFunc, false);}catch(err){};
  try{document.querySelectorAll('.dataTables_wrapper')[5].removeEventListener("scroll", myTableSideScrollFunc, false);}catch(err){};
  try{document.querySelectorAll('.dataTables_wrapper')[5].addEventListener("scroll", myTableSideScrollFunc, false);}catch(err){};
},1000);

function myTableSideScrollFunc(e){
	console.log('SIDE SCROLL YEAH! '+e.target.className);
	var maxCols = e.target.parentElement.parentElement.parentElement.className.split(' d')[1][0][0]; //[1]);  //.querySelector('.table_a')
	//console.log(e.target.scrollLeft);
	if(e.target.scrollLeft > 0){
		ancestorElement(e.target,3).querySelector('.table_a').className = 'table_a scrolled';
	}
	else{
		ancestorElement(e.target,3).querySelector('.table_a').className = 'table_a';
	}

	//document.querySelector('.mySortHeaderSet>div:nth-child(4)').style.left = document.querySelectorAll('.dataTables_wrapper')[1].scrollLeft+'px';
	  //document.querySelector('.mySortHeaderSetMetrics').style.left = -document.querySelectorAll('.dataTables_wrapper')[1].scrollLeft+'px';
	//document.querySelector('table thead th:nth-child(6)').getBoundingClientRect().left+'px';
}
*/


//MAKE OUR OWN SORTING BUTTONS FOR COLUMNS, MASKING THE REAL ONES, ALLOWING US TO MULTI-SORT IN ONE CLICK TO HELP DRILLDOWNS
/*
var sortHeaderMaker = setInterval(function(){
//function sortHeaderMaker(){
	var t = document.querySelector('.dataTables_wrapper').querySelectorAll('th');
	var c = [];
	var looper = 0;
	if(t.length>0 && document.querySelectorAll('.mySortHeaderSet').length==0){
		var innerLoop = setInterval(function(){
			var d = document.createElement('div');
			d.className = 'mySortHeaderSet';
			var d2 = document.createElement('div');
			d2.className = 'mySortHeaderSetMetrics';

			var bindElement = document.querySelector('.dataTables_wrapper');

			var pad_left = document.createElement('div');
			pad_left.style.position = 'absolute';
			pad_left.className = 'pad_left sort_header_col';
			pad_left.style.width = '20px';
			pad_left.style.height = t[1].getBoundingClientRect().height+'px';
			pad_left.style.backgroundColor = '#F9F9F9';
			pad_left.style.top = (t[1].getBoundingClientRect().top - bindElement.getBoundingClientRect().top) + 'px';
			pad_left.style.left= (t[1].getBoundingClientRect().left-bindElement.getBoundingClientRect().left-20)+'px';
			pad_left.style.border='none';
			d.appendChild(pad_left);

			for(i=0;i<t.length;i++){
				//console.log(t[i].getBoundingClientRect())
				if(t[i].getBoundingClientRect().top>0){
					c[i] = document.createElement('div');
					c[i].style.position = 'absolute';
					c[i].className = 'tablecol_'+i+' '+t[i].innerText+' sort_header_col';
					c[i].style.width = t[i].getBoundingClientRect().width+'px'; //(i==1?20:0))+'px';
					c[i].style.height = t[i].getBoundingClientRect().height+'px';
					c[i].style.backgroundColor = '#F9F9F9';   //'#F'+Math.floor(Math.random()*10)+'0';
					//c[i].style.opacity = '0.7';
					c[i].style.top = (t[i].getBoundingClientRect().top - bindElement.getBoundingClientRect().top) + 'px';
					c[i].style.left= (t[i].getBoundingClientRect().left- bindElement.getBoundingClientRect().left) + 'px'; //-(i==1?20:0))+ 'px';
					df = document.querySelectorAll('.drilldown_false').length;
					if(i>1 || df>0){c[i].innerHTML = t[i].innerHTML;} //'Test';
					if(i<5){d.appendChild(c[i]);}else{d2.appendChild(c[i]);}
				}
			}
			d.appendChild(d2);

			var pad_right = document.createElement('div');
			pad_right.style.position = 'fixed';
			pad_right.className = 'pad_right sort_header_col';
			pad_right.style.width = '20px';
			pad_right.style.height = (8+t[1].getBoundingClientRect().height)+'px';
			pad_right.style.backgroundColor = '#F9F9F9';
			pad_right.style.top = '54px'; //(t[1].getBoundingClientRect().top - bindElement.getBoundingClientRect().top) + 'px';
			pad_right.style.right = '0px'; //(t[1].getBoundingClientRect().left-bindElement.getBoundingClientRect().left-20)+'px';
			pad_right.style.border='none';
			d.appendChild(pad_right);

			try{bindElement.removeChild(bindElement.querySelector('.mySortHeaderSet'));}catch(e){}
			bindElement.appendChild(d);
			looper++;
			if(looper==2){clearInterval(innerLoop);}
		},250);
		//clearInterval(sortHeaderMaker);
	}
},250)
*/




/*
function tidyWidths(){
	var wid = 0;for(i=0;i<document.querySelectorAll('.col_dims').length;i++){
		if(document.querySelectorAll('.col_dims')[i].offsetWidth > wid){
			wid = document.querySelectorAll('.col_dims')[i].offsetWidth;
		}
	}
	document.querySelector('.dataTables_wrapper').style.marginLeft = (wid+70)+'px';
}

//ACTIVATE THE SORT!
var sortActions = function(t,i){
	var tindex = '';
	for(j=0;j<10;j++){
		if(ancestorElement(t,3)==document.querySelectorAll('table')[j]){tindex = Math.floor(j/2);break;}
	}

	var table1 = $('#'+document.querySelectorAll('table[id^="DataTables_Table_"]')[tindex*2].id).DataTable(); //$('#DataTables_Table_0').DataTable();
	var table2 = $('#'+document.querySelectorAll('table[id^="DataTables_Table_"]')[1+(tindex*2)].id).DataTable(); 
	//var sortType = (i==dataLayer[0].sortCol?dataLayer[0].sortType:0)||0;
	try{var sortDir = i==dataLayer[0].sortCol[0]?(dataLayer[0].sortCol[1]=='asc'?'desc':'asc'):'desc';}catch(e){var sortDir = 'desc';}
	table1
	    .order([i,sortDir])  //([colIndex+2-sortType,'asc'])
	    .draw();
	table2
	    .order([i,sortDir])
	    .draw();
	dataLayer[0].sortCol = [i,sortDir]; //colIndex;
	//dataLayer[0].sortType = 1-sortType; //!dataLayer[0].sortType?1:0;
	//dataLayer[0].sortDir = 
	getIndices();
	//console.log('Col '+colIndex+' sorted '+(!dataLayer[0].sortType?'asc':'desc')+'!');
}

//HERE WE OBSERVE WHEN THE REFRESHING OF THE TABLE COMPLETES EACH TIME IT IS LOADED/ASJUSTED, TO ENSURE DRILLDOWN INDICES ARE CORRECT
Shiny.addCustomMessageHandler('update',detectTableRefresh);
function detectTableRefresh(msg){
	console.log('refresh');
	var a = 0;
    var detectTableShrink = setInterval(function(){
        if(document.querySelectorAll('table tr').length==1 || a==250){  //table shrinks to just header row when rebuilding
			tableGrow();clearInterval(detectTableShrink);
		}a++;
    },20)
    function tableGrow(){
		var detectTableGrow = setInterval(function(){
			if(document.querySelectorAll('table tr').length>1 && document.querySelector('table').getBoundingClientRect().height > 200){ //all rows reappear, so we know table is ready to 'get' properties from
				//setTimeout(function(){tidyWidths();},750);
				clearInterval(detectTableGrow);
			}
		},20);
	}
}
*/