var numericInput2 = new Shiny.InputBinding();
$.extend(numericInput2, {
  find: function(scope) {
    return $(scope).find(".shiny-numeric-input-2");
  },
  getValue: function(el) {
    var $inputs = $(el).find("input");
    var $useRange = $inputs[0].getAttribute("data-display-if");

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
  setValue: function(el, value) {
    if (value.length == 1) {
        el.find("input")[0].value = value;
    } else {
        el.find("input")[1].value = value[0];
        el.find("input")[2].value = value[1];
    }

  },
  receiveMessage: function(el, data) {
    var $el = $(el);

    if (data.hasOwnProperty("label")) {
        $el.find("label[for='" + Shiny.$escape(el.id) + "']").text(data.label);
    }

    if (data.hasOwnProperty("value")) {
        this.setValue($el, data.value);
    }

    $(el).trigger("change");
  },
  subscribe: function(el, callback) {
    $(el).on('change.numericInput2', function(e) {
      callback();
    });

  },
  unsubscribe: function(el) {
    $(el).off('.numericInput2');
  }
});

Shiny.inputBindings.register(numericInput2, 'shiny.numericInput2');
