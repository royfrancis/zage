

$(document).ready(function() {  
    var table = $('#DataTables_Table_0').DataTable( {     
        "ajax": 'https://api.myjson.com/bins/qgcu',
        "initComplete": function(settings){
            $('#DataTables_Table_0 thead th').each(function () {
               var $td = $(this);
               $td.attr('title', $td.text());
            });

            /* Apply the tooltips */
            $('#DataTables_Table_0 thead th[title]').tooltip(
            {
               "container": 'body'
            });          
        }  
    }); 
});
