$.ajax({
	url: "/hello",
       }).done(function() { 
	       $(this).addClass("done");
	       alert("test");
	  });