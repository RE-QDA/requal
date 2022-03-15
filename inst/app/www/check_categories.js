function check_categories(evt, el) {
     // console.log(evt.clone);
     // console.log(evt.item.closest('.category-container'));
     var dragged_code_id = evt.clone.getElementsByClassName('code_item').item(0).getAttribute('data-code_id'); 
     var dragged_code_id = Number(dragged_code_id);
     
     var dragged_category_id = evt.item.closest('.category-container').getAttribute('data-category_id');
     var dragged_category_id = Number(dragged_category_id);
     
     var existing_codes = evt.item.closest('.category-container').getElementsByClassName("code_item");
     // console.log(existing_codes);

     var existing_code_ids = [];
     // console.log(existing_code_ids);
     
     // write existing codes ids to array
     for (let i = 0; i < existing_codes.length; i++) {
            existing_code_ids[i] = Number(existing_codes.item(i).getAttribute('data-code_id'));
        }
        
     // console.log(existing_code_ids);
     // console.log(dragged_code_id);
     
     // get an array of unique code ids
     var unique = existing_code_ids.filter((item, i, ar) => ar.indexOf(item) === i);
     // console.log(unique);


     if (existing_code_ids.length > unique.length){ // if the code is present, cancel action
         
        el.removeChild(evt.item);
        return;
        
        } else { // if the code is missing, set input values
            
            // categories_ui_1- must be prepended to match module name
            Shiny.setInputValue("categories_ui_1-edges_category", {category_id: dragged_category_id, code_id: dragged_code_id});
            return;
            
        }
}

function check_categories_delete(evt, el) {


     var dragged_code_id = evt.clone.getElementsByClassName('code_item').item(0).getAttribute('data-code_id'); 
     var dragged_code_id = Number(dragged_code_id);
     
     previous_category_id = Number(evt.from.closest('.category-container').getAttribute('data-category_id'));
     
     // categories_ui_1- must be prepended to match module name
     Shiny.setInputValue("categories_ui_1-edges_category_delete", {category_id: previous_category_id, code_id: dragged_code_id});

     el.removeChild(evt.item);
   
}