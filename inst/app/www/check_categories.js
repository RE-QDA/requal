function check_categories(evt) {
     
     var dragged_code_id = evt.item.getElementsByClassName('code_item').item(0).getAttribute('data-code_id'); 
     var dragged_code_id = Number(dragged_code_id);
     
     var dragged_category_id = evt.item.getElementsByClassName('code_item').item(0).closest('.category-container').getAttribute('data-category_id');
     var dragged_category_id = Number(dragged_category_id);
     
     var preexisting_codes = evt.item.getElementsByClassName('code_item').item(0).closest('.category-rank-list').getElementsByClassName("code_item");
     
     var preexisting_code_ids = [];
     console.log(preexisting_code_ids);

     for (let i = 0; i < preexisting_codes.length; i++) {

            preexisting_code_ids[i] = Number(preexisting_codes.item(i).getAttribute('data-code_id'));

        }
        
     console.log(preexisting_code_ids);
     console.log(dragged_code_id);


     var check = preexisting_code_ids.includes(dragged_code_id);
     console.log(check);
}

