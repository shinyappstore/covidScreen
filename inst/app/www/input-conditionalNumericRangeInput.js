var conditionalNumericRangeInput = new Shiny.InputBinding();
$.extend(conditionalNumericRangeInput, {
  find: function(scope) {
    return $(scope).find(".shiny-conditional-numeric-range-input");
  },
  getType: function(el) {
    return "covidtest.conditionalRangeInput";
  },
  getValue: function(el) {
    // Find inputs in element
    var $inputs = $(el).find("input");

    // Get input values
    var point = $inputs[0].value;
    var start = $inputs[1].value;
    var end = $inputs[2].value;

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
      inputs[1].value = value[0];
      inputs[2].value = value[1];
    }
  },
  subscribe: function(el, callback) {
    $(el).on("change.conditionalNumericRangeInput", function(e) {
      callback();
    });
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
  unsubscribe: function(el) {
    $(el).off(".conditionalNumericRangeInput");
  }
});

Shiny.inputBindings.register(
  conditionalNumericRangeInput,
  "covidtest.conditionalNumericRangeInput"
);
