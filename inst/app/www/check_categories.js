function check_categories(evt, el) {
     
     var dragged_code_id = evt.item.getElementsByClassName('code_item').item(0).getAttribute('data-code_id'); 
     var dragged_code_id = Number(dragged_code_id);
     
     var dragged_category_id = evt.item.getElementsByClassName('code_item').item(0).closest('.category-container').getAttribute('data-category_id');
     var dragged_category_id = Number(dragged_category_id);
     
     var preexisting_codes = evt.item.getElementsByClassName('code_item').item(0).closest('.category-rank-list').getElementsByClassName("code_item");
     
     var id_exists = 0;

     for (let i = 0; i < preexisting_codes.length; i++) {

            if (dragged_code_id === Number(preexisting_codes.item(i).getAttribute('data-code_id'))) {
                
                     var id_exists = id_exists+i;
            }

        }

     var check = id_exists > 0;

     if (check) { // if the code is present, cancel action
         
        el.removeChild(evt.item);
        return;
        
        } else { // if the code is missing, set input values
            
            console.log(dragged_code_id)
            // mod_categories_ui_1-
            Shiny.setInputValue("edge_category", dragged_code_id);
            
        }
}

