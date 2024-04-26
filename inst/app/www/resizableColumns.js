$(document).ready(function() {
    (function($, window, document, undefined) {
  
      $.widget('ce.resizableGrid', {
  
        _create: function() {
          this.resizing = false;
          this._on({
            'mousedown .resizable-column-handle': '_resizeStartHandler',
            'mousemove': '_resizeHandler',
            'mouseup': '_resizeStopHandler',
            'mouseleave': '_resizeStopHandler'
          });
        },
  
        _init: function() {
          this._createHelpers();
        },
  
        _createHelpers: function() {
          this.element.addClass('resizable-grid');
          this.element.find('> .row:not(.resizable-row)').each(function(rowIndex, rowElement) {
            var row = $(rowElement);
            row.addClass('resizable-row');
            row.find('> [class^="col-"]:not(.resizable-column)').each(function(columnIndex, columnElement) {
              var column = $(columnElement);
              column.addClass('resizable-column');
              column.append(
                $('<div>', {
                  class: 'resizable-column-handle resizable-column-handle-w',
                  'data-is-west': 'true'
                }),
                $('<div>', {
                  class: 'resizable-column-handle resizable-column-handle-e',
                  'data-is-west': 'false'
                })
              );
            });
          });
        },
  
        _resizeStartHandler: function(event) {
          this.resizing = {};
          this.resizing.handle = $(event.currentTarget).addClass('resizable-column-handle-resizing');
          this.resizing.column = this.resizing.handle.closest('.resizable-column').addClass('resizable-column-resizing');
          this.resizing.row = this.resizing.column.closest('.resizable-row').addClass('resizable-row-resizing');
          this.resizing.handleIsWest = this.resizing.handle.data('isWest');
          this.resizing.directionIsWest = this._getResizingDirectionIsWest(event.pageX);
          this.resizing.columnSize = this._getColumnSize(this.resizing.column);
          this.resizing.siblings = this._getResizingSiblings(this.resizing.column);
          this.resizing.offsets = this._getResizingOffsets();
          this.element.addClass('resizable-grid-resizing');
        },
  
        _resizeHandler: function(event) {
          if(!this.resizing) {
            return;
          }
          this.resizing.directionIsWest = this._getResizingDirectionIsWest(event.pageX);
          var resizingOffsetSize = this._getResizingOffsetSize(event.pageX);
          if(resizingOffsetSize && (this.resizing.columnSize !== resizingOffsetSize)) {
            if(resizingOffsetSize > this.resizing.columnSize) {
              var widestColumn = this._getWidestColumn(this.resizing.siblings),
                widestColumnSize = this._getColumnSize(widestColumn);
              this._setColumnSize(widestColumn, (widestColumnSize - 1));
              this._setColumnSize(this.resizing.column, resizingOffsetSize);
            } else {
              var narrowestColumn = this._getNarrowestColumn(this.resizing.siblings),
                narrowestColumnSize = this._getColumnSize(narrowestColumn);
              this._setColumnSize(narrowestColumn, (narrowestColumnSize + 1));
              this._setColumnSize(this.resizing.column, resizingOffsetSize);
            }
            this.resizing.columnSize = resizingOffsetSize;
          }
        },
  
        _resizeStopHandler: function(event) {
          if(!this.resizing) {
            return;
          }
          this.resizing.handle.removeClass('resizable-column-handle-resizing');
          this.resizing.column.removeClass('resizable-column-resizing');
          this.resizing.row.removeClass('resizable-row-resizing');
          this.element.removeClass('resizable-grid-resizing');
          this.resizing = false;
        },
  
        _getResizingDirectionIsWest: function(x) {
          var resizingDirectionIsWest;
          if(!this.resizing.directionLastX) {
            this.resizing.directionLastX = x;
            resizingDirectionIsWest = null;
          } else {
            if(x < this.resizing.directionLastX) {
              resizingDirectionIsWest = true;
            } else {
              resizingDirectionIsWest = false;
            }
            this.resizing.directionLastX = x;
          }
          return resizingDirectionIsWest;
        },
  
        _getResizingSiblings: function(column) {
          return ((this.resizing.handleIsWest) ? column.prevAll() : column.nextAll());
        },
  
        _getResizingOffsetSize: function(x) {
          var that = this,
            resizingOffsetSize;
          $.each(this.resizing.offsets, function(index, offset) {
            if((that.resizing.directionIsWest && ((x <= offset.end) && (x >= offset.start))) || (!that.resizing.directionIsWest && ((x >= offset.start) && (x <= offset.end)))) {
              resizingOffsetSize = offset.size;
            }
          });
          return resizingOffsetSize;
        },
  
        _getResizingOffsets: function() {
          var that = this,
            row = this.resizing.row.clone(),
            css = {
              'height': '1px',
              'min-height': '1px',
              'max-height': '1px'
            };
          row.removeClass('resizable-row resizable-row-resizing').css(css);
          row.children().empty().removeClass('resizable-column resizable-column-resizing').css(css);
          this.resizing.row.parent().append(row);
          var column = row.children().eq(this.resizing.row.children().index(this.resizing.column)),
            totalSize = this._getColumnSize(column);
          this._getResizingSiblings(column).each(function() {
            totalSize += (that._getColumnSize($(this)) - 1);
            that._setColumnSize($(this), 1);
          });
          var size = ((this.resizing.handleIsWest) ? totalSize : 1),
            sizeEnd = ((this.resizing.handleIsWest) ? 1 : totalSize),
            sizeOperator = ((this.resizing.handleIsWest) ? -1 : 1),
            offset = 0,
            offsetOperator = ((this.resizing.handleIsWest) ? 1 : 0);
          var columnGutter = ((column.outerWidth(true) - column.width()) / 2),
            columnWidth = ((this.resizing.handleIsWest) ? false : true);
          var resizingOffsets = [];
          while(true) {
            this._setColumnSize(column, size);
            this._setColumnOffset(column, offset);
            var left = (Math.floor((column.offset()).left) + columnGutter + ((columnWidth) ? column.width() : 0));
            resizingOffsets.push({
              start: (left + ((columnGutter * 3) * -1)),
              end: (left + (columnGutter * 3)),
              size: size
            });
            if(size === sizeEnd) {
              break;
            }
            size += sizeOperator;
            offset += offsetOperator;
          }
          row.remove();
          return resizingOffsets;
        },
  
        _getWidestColumn: function(columns) {
          var that = this,
            widestColumn;
          columns.each(function() {
            if(!widestColumn || (that._getColumnSize($(this)) > that._getColumnSize(widestColumn))) {
              widestColumn = $(this);
            }
          });
          return widestColumn;
        },
  
        _getNarrowestColumn: function(columns) {
          var that = this,
            narrowestColumn;
          columns.each(function() {
            if(!narrowestColumn || (that._getColumnSize($(this)) < that._getColumnSize(narrowestColumn))) {
              narrowestColumn = $(this);
            }
          });
          return narrowestColumn;
        },
  
        _getColumnSize: function(column) {
          var columnSize;
          $.each($.trim(column.attr('class')).split(' '), function(index, value) {
            if(value.match(/^col-/) && !value.match(/-offset-/)) {
              columnSize = parseInt($.trim(value).replace(/\D/g, ''), 10);
            }
          });
          return columnSize;
        },
  
        _setColumnSize: function(column, size) {
          column.toggleClass([
            ['col', 'sm', this._getColumnSize(column)].join('-'), ['col', 'sm', size].join('-')
          ].join(' '));
        },
  
        _getColumnOffset: function(column) {
          var columnOffset;
          $.each($.trim(column.attr('class')).split(' '), function(index, value) {
            if(value.match(/^col-/) && value.match(/-offset-/)) {
              columnOffset = parseInt($.trim(value).replace(/\D/g, ''), 10);
            }
          });
          return columnOffset;
        },
  
        _setColumnOffset: function(column, offset) {
          var currentColumnOffset,
            toggleClasses = [];
          if((currentColumnOffset = this._getColumnOffset(column)) !== undefined) {
            toggleClasses.push(['col', 'sm', 'offset', currentColumnOffset].join('-'));
          }
          toggleClasses.push(['col', 'sm', 'offset', offset].join('-'));
          column.toggleClass(toggleClasses.join(' '));
        },
  
        _destroy: function() {
          this._destroyHelpers();
        },
  
        _destroyHelpers: function() {
          this.element.find('.resizable-column-handle').remove();
          this.element.find('.resizable-column').removeClass('resizable-column resizable-column-resizing');
          this.element.find('.resizable-row').removeClass('resizable-row resizable-row-resizing');
          this.element.removeClass('resizable-grid resizable-grid-resizing');
        }
      });
  
    })(jQuery, window, document);
  
    $('#layout').resizableGrid();
  
  });