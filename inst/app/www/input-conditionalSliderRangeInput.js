var conditionalSliderRangeInput = new Shiny.InputBinding();
$.extend(conditionalSliderRangeInput, {
  find: function(scope) {
    return $(scope).find('.shiny-conditional-slider-range-input');
  },
  getType: function(el) {
    return "covidscreen.conditionalRangeInput";
  },
  getValue: function(el) {
    // Find inputs in element
    var $inputs = $(el).find("input");

    // Get input values
    var point = $inputs[0].value;
    var rangeString = $inputs[1].value;

    var start = rangeString.replace(/[;].*$/, '')
    var end = rangeString.replace(/^.*[;]/, '')

    // Coerce values
    if (/^\s*$/.test(point)) {
      // Return null if all whitespace
      point = null;
    } else if (!isNaN(point)) {
      // If valid Javascript number string, coerce to number
      point = +point;
    } else {
      point = point;
    }
    if (/^\s*$/.test(start)) {
      // Return null if all whitespace
      start = null;
    } else if (!isNaN(start)) {
      // If valid Javascript number string, coerce to number
      start = +start;
    } else {
      start = start;
    }
    if (/^\s*$/.test(end)) {
      // Return null if all whitespace
      end = null;
    } else if (!isNaN(end)) {
      // If valid Javascript number string, coerce to number
      end = +end;
    } else {
      end = end;
    };

    // Input ID without prefix
    var inputId = el.id.replace(el.dataset.nsPrefix, '');
    var condVar = el.dataset.conditionVariable;

    return {
      "values": {"point": point, "range": [start, end]},
      "attributes": {"inputId": inputId, "trigger": condVar}
    };
  },
  setValue: function(el, value) {
    // Find inputs
    var inputs = el.find("input");

    // Assign to correct values
    if (value.length == 1) {
      inputs[0] = value;
    } else {
      inputs[1].value = value;
    }
  },
  receiveMessage: function(el, data) {
    var $el = $(el);

    if (data.hasOwnProperty("label")) {
      $el.find('label[for="' + Shiny.$escape(el.id) + '"]').text(data.label);
    }

    if (data.hasOwnProperty("value")) {
      this.setValue($el, data.value);
    }

    $(el).trigger("change");
  },
  subscribe: function(el, callback) {
    $(el).on('click.conditionalSliderRangeInput', function(e) {
      callback();
    });

  },
  unsubscribe: function(el) {
    $(el).off('.conditionalSliderRangeInput');
  }
});

Shiny.inputBindings.register(
  conditionalSliderRangeInput,
  'covidscreen.conditionalSliderRangeInput'
);
