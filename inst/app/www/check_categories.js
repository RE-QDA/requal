function check_categories(evt, el) {
     console.log(evt.clone);
     var dragged_code_id = evt.clone.getElementsByClassName('code_item').item(0).getAttribute('data-code_id'); 
     var dragged_code_id = Number(dragged_code_id);
     
     var dragged_category_id = evt.item.closest('.category-container').getAttribute('data-category_id');
     var dragged_category_id = Number(dragged_category_id);
     
     var preexisting_codes = evt.item.closest('.category-rank-list').getElementsByClassName("code_item");
     
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
            
            // categories_ui_1- must be prepended to match module name
            Shiny.setInputValue("categories_ui_1-edges_category", {category_id: dragged_category_id, code_id: dragged_code_id});
            return;
            
        }
}

