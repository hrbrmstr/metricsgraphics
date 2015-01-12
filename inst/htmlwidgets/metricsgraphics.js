HTMLWidgets.widget({

  name: 'metricsgraphics',

  type: 'output',

  initialize: function(el, width, height) {

    return {  }

  },

  renderValue: function(el, params, instance) {

    // save params for reference from resize method
    instance.params = params;

    // draw the graphic
    this.drawGraphic(el, params, el.offsetWidth, el.offsetHeight);

  },

  drawGraphic: function(el, params, width, height) {

    // remove existing children
    while (el.firstChild)
      el.removeChild(el.firstChild);

    dbg = params

    wide = null;

    is_multi_line = params.y_accessor instanceof Array;

    if (params.geom == "histogram") {
      wide = params.data;
    } else {

      wide = HTMLWidgets.dataframeToD3(params.data);

      if (is_multi_line) {

        tmp = [];

        for (var i=0; i<params.y_accessor.length; i++) {
          var data = {};
          data["value"]= params.data[params.y_accessor[i]];
          data[params.x_accessor]  = params.data[params.x_accessor];
          tmp.push(HTMLWidgets.dataframeToD3(data));
        }

        wide = tmp ;

      }
    }

    var xax_format = mjs_plain;

    if (params.xax_format == "date") {

      xax_format = mjs_date ;

      if (!is_multi_line) {
        MG.convert.date(wide, params.x_accessor)
      } else {
        for (var i=0; i<wide.length; i++) {
          wide[i] = MG.convert.date(wide[i], params.x_accessor);
        }
      }

      if (params.markers != null) {
        for (var i=0; i<params.markers.length; i++) {
          params.markers[i][params.x_accessor] =
            d3.time.format("%Y-%m-%d").parse(params.markers[i][params.x_accessor]);
        }
      }

    }

    if (params.xax_format == "comma") xax_format = mjs_comma ;

    var mg_params = {

        data: wide,
        target: '#' + el.id,
        xax_format: xax_format,

        x_axis: params.x_axis,
        y_axis: params.y_axis,

        interpolate: params.interpolate,
        decimals: params.decimals,
        format: params.format,

        color_type: params.color_type,
        color_range: params.color_range,
        point_size: params.point_size,
        size_range: params.size_range,

        markers: params.markers,
        baselines: params.baselines,

        show_rollover_text: params.show_rollover_text,

        y_scale_type: params.y_scale_type,

        chart_type: params.chart_type,
        x_accessor: params.x_accessor,
        y_accessor: params.y_accessor,
        color_accessor: params.color_accessor,
        size_accessor: params.size_accessor,

        show_rollover_text: params.show_rollover_text,

        legend: params.legend,
        legend_target: params.legend_target,

        bar_margin: params.bar_margin,
        binned: params.binned,
        bins: params.bins,

        linked: params.linked,

        height: height,
        width: width,
        bottom: params.bottom,
        top: params.top,
        right: params.right,
        left: params.left,
        buffer: params.buffer,

        area: params.area,
        animate_on_load: params.animate_on_load,
        y_rug: params.y_rug,
        x_rug: params.x_rug,

        min_x: params.min_x,
        max_x: params.max_x,
        min_y: params.min_y,
        max_y: params.max_y,

        yax_count: params.yax_count,
        xax_count: params.xax_count,

        least_squares: params.least_squares,

        x_label: params.x_label,
        y_label: params.y_label,
        title: params.title,
        description: params.description

    };

    if (!is_multi_line) {

      MG.data_graphic(mg_params);

    } else {

      mg_params.y_accessor = "value";

      if (!params.grid) {

        //draw a multiline chart
        MG.data_graphic(mg_params);

      } else {

        var n = params.y_accessor.length;

        // arrange sizes
        mg_params.width  = width/n;
        mg_params.height = height/n;
        mg_params.bottom = params.bottom/n + 10;
        mg_params.top    = params.top/n + 10;
        mg_params.right  = params.right/n;
        mg_params.left   = params.left/n;
        mg_params.buffer = params.buffer/n;
        mg_params.yax_count = Math.ceil(params.yax_count/n);
        mg_params.xax_count = Math.ceil(params.xax_count/n);

        mg_params.linked = true;

        var parentDom = document.querySelector(mg_params.target);

        for (var i=0; i<n; i++) {
          var y_accessor = params.y_accessor[i];
          var div = document.createElement("div");
          div.id = y_accessor;
          div.style.float = "left";
          parentDom.appendChild(div);

          mg_params.data   = wide[i];
          mg_params.target = "#" + y_accessor;

          // add titles instead of legends
          if (mg_params.legend instanceof Array){
            mg_params.title = params.legend[i];
          }
          mg_params.legend_target = null;

          //draw a separate chart
          MG.data_graphic(mg_params);
        }

      }
    }

  },

  resize: function(el, width, height, instance) {
    if (instance.params)
      this.drawGraphic(el, instance.params, width, height);
  }

});

mjs_comma = function(d) {
  var df =  d3.format("0,000");
  return df(d);
}

mjs_date = function(d) {
  var df = d3.time.format('%b %d');
  return df(d);
}

mjs_plain = function(d) {
 return(d.toString());
}
