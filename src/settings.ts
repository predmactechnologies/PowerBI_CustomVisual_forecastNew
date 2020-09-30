
"use strict";
import { dataViewObjectsParser } from "powerbi-visuals-utils-dataviewutils";
import DataViewObjectsParser = dataViewObjectsParser.DataViewObjectsParser;

export class VisualSettings extends DataViewObjectsParser {
      public settings_forecast_params: settings_forecast_params = new settings_forecast_params();
      public settings_plot_params: settings_plot_params = new settings_plot_params();
      public settings_axis_params: settings_axis_params = new settings_axis_params();
      }

    export class settings_forecast_params {
      public conf1: string = "Forecast Next 30 Days";
      public conf2: string = "Forecast Next 30 Days";
    }
      export class settings_plot_params {
      public plot1: string = "line";
    }

    export class settings_axis_params {
      public xaxis1: string = "Date";
      public yaxis1: string = "Value";
    }

