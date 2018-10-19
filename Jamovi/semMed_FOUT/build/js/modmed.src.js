
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"dep","title":"Dependent Variable","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a string naming the dependent variable"}},{"name":"meds","title":"Mediators","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a string naming the mediator variable"}},{"name":"pred","title":"Predictor","type":"Variable","suggested":["continuous"],"permitted":["numeric","factor"],"description":{"R":"a string naming the predictor variable"}},{"name":"xmmod","title":"Moderator a-path","type":"Variable","suggested":["continuous"],"permitted":["numeric","factor"],"description":{"R":"a string naming the predictor variable"}},{"name":"mymod","title":"Moderator b-path","type":"Variable","suggested":["continuous"],"permitted":["numeric","factor"],"description":{"R":"a string naming the predictor variable"}},{"name":"covsm","title":"Covariates mediators","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a vector with strings naming the covariates"}},{"name":"covsy","title":"Covariates dependent","type":"Variables","suggested":["continuous"],"permitted":["numeric"],"description":{"R":"a vector with strings naming the covariates"}},{"name":"estMethod","title":"Estimation Method for SE's","type":"List","options":[{"name":"standard","title":"Standard"},{"name":"bootstrap","title":"Bootstrap"}],"default":"standard","description":{"R":"`'standard'` (default), or `'bootstrap'`, the estimation method to use\n"}},{"name":"bootstrap","title":"Samples","type":"Integer","min":1,"default":1000,"description":{"R":"a number between 1 and 100000 (default: 1000) specifying the number of  samples that need to been drawn in the bootstrap method\n"}},{"name":"test","title":"Test statistics","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, provide 'Z' and 'p' values for the mediation estimates\n"}},{"name":"ci","title":"Confidence interval","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a confidence interval for the mediation estimates\n"}},{"name":"ciWidth","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width that is used as `'ci'`\n"}},{"name":"pm","title":"Percent mediation","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide the percent mediation  effect size for the mediation estimates\n"}},{"name":"paths","title":"Path estimates","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide the individual estimates of the  paths in the mediation model\n"}},{"name":"label","title":"Labels","type":"Bool","default":false,"description":{"R":"`TRUE` (default) or `FALSE`, provide insightful labels for all estimates\n"}},{"name":"estPlot","title":"Estimate plot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide an estimate plot where for each estimator the estimated coefficient and confidence intervals are plotted.\n"}}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Moderated mediation",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Mediators",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "meds",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Predictor",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "pred",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Moderator a-path",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "xmmod",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Moderator b-path",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "mymod",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Covariates mediators",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "covsm",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Covariates dependent",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "covsy",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.ComboBox,
					name: "estMethod"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					name: "bootstrap",
					format: FormatDef.number,
					inputPattern: "[0-9]+"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					name: "test"
				},
				{
					type: DefaultControls.CheckBox,
					name: "ci"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					name: "ciWidth",
					format: FormatDef.number,
					inputPattern: "[0-9]+"
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.CheckBox,
					name: "pm"
				},
				{
					type: DefaultControls.CheckBox,
					name: "paths"
				},
				{
					type: DefaultControls.CheckBox,
					name: "label"
				},
				{
					type: DefaultControls.CheckBox,
					name: "estPlot"
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
