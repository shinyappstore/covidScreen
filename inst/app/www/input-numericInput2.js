// Create a new input binding
var numericInput2 = new Shiny.InputBinding();

// Define methods
$.extend(numericInput2, {
  // Find instances in document
  find: function(scope) {
    return $(scope).find(".shiny-numeric-input-2");
  },
  // Get current value of instance (element)
  getValue: function(el) {
    // Find all input tags (3 of them)
    var $inputs = $(el).find("input");
    // Determine whether single value or range is currently in use
    var $useRange = $inputs[0].getAttribute("data-display-if");
    // Get input values
    var single = $inputs[0].value;
    var start = $inputs[1].value;
    var end = $inputs[2].value;

    // Handle special cases for single
    if (!$useRange && /^\s*$/.test(single)) {
        single = null;
    } else if (!$useRange && !isNaN(single)) {
        single = +single;
    } else {
        single = single;
    }

    // Handle special cases for start
    if ($useRange && /^\s*$/.test(start)) {
        start = null;
    } else if ($useRange && !isNaN(start)) {
        start = +start;
    } else {
        start = start;
    }

    // Handle special cases for end
    if ($useRange && /^\s*$/.test(end)) {
        end = null;
    } else if ($useRange && !isNaN(end)) {
        end = +end;
    } else {
        end = end;
    }

    // Return either single number or range
    if ($useRange) {
        return [start, end];
    } else {
        return single;
    }
  },
  // Set the value(s) of an instance (element)
  setValue: function(el, value) {
    if (value.length == 1) {
        el.find("input")[0].value = value;
    } else {
        el.find("input")[1].value = value[0];
        el.find("input")[2].value = value[1];
    }

  },
  // Handle update messages
  receiveMessage: function(el, data) {
    var $el = $(el);
    // Update label
    if (data.hasOwnProperty("label")) {
        $el.find("label[for='" + Shiny.$escape(el.id) + "']").text(data.label);
    }
    // Update value(s)
    if (data.hasOwnProperty("value")) {
        this.setValue($el, data.value);
    }
    // Signal that something has changed
    $(el).trigger("change");
  },
  // Define listeners
  subscribe: function(el, callback) {
    // Listen for changes to input (rate-limited)
    $(el).on('change.numericInput2', function(e) {
      callback(true);
    });
    // Listen for unfocus (not rate-limited)
    $el.on('blur.numericInput2', function(e) {
      callback();
    });
  },
  // Stop listening
  unsubscribe: function(el) {
    $(el).off('.numericInput2');
  }
});

// Register input binding
Shiny.inputBindings.register(numericInput2, 'shiny.numericInput2');
