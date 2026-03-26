const formatters = {
  kable: { name: 'kable', parent: 'knitr' },
  kable_styling: { name: 'kable_styling', parent: 'knitr' },

  kbl: { name: 'kbl', parent: 'kableExtra' },
  xtable2kable: { name: 'xtable2kable', parent: 'kableExtra' },

  xtable: { name: 'xtable', parent: 'xtable' },
  xtableList: { name: 'xtableList', parent: 'xtable' },
  'print.xtable': { name: 'xtable', parent: 'xtable' },
  'print.xtableList': { name: 'xtableList', parent: 'xtable' },

  formattable: { name: 'formattable', parent: 'formattable' },

  gt: { name: 'gt', parent: 'gt' },

  datatable: { name: 'datatable', parent: 'DT' },

  'pandoc.table': { name: 'pandoc.table', parent: 'pander' },
  'pandoc.list': { name: 'pandoc.list', parent: 'pander' },
  'pander': { name: 'pander', parent: 'pander' },

  hux: { name: 'hux', parent: 'huxtable' },

  reactable: { name: 'reactable', parent: 'reactable' },
  flextable: { name: 'flextable', parent: 'flextable' },

  dust: { name: 'dust', parent: 'pixiedust' },

  tangram: { name: 'tangram', parent: 'tangram' },

  ztable: { name: 'ztable', parent: 'ztable' },

  condformat: { name: 'condformat', parent: 'condformat' },

  stargazer: { name: 'stargazer', parent: 'stargazer' },

  tbl_summary: { name: 'tbl_summary', parent: 'gtsummary' },

  htmlTable: { name: 'htmlTable', parent: 'htmlTable' },
  tidyHtmlTable: { name: 'tidyHtmlTable', parent: 'htmlTable' },

  modelsummary: { name: 'modelsummary', parent: 'modelsummary' },
  modelplot: { name: 'modelplot', parent: 'modelsummary' },
  datasummary: { name: 'datasummary', parent: 'modelsummary' },
  datasummary_crosstab: { name: 'datasummary_crosstab', parent: 'modelsummary' },
  datasummary_balance: { name: 'datasummary_balance', parent: 'modelsummary' },
  datasummary_correlation: { name: 'datasummary_correlation', parent: 'modelsummary' },
  datasummary_skim: { name: 'datasummary_skim', parent: 'modelsummary' },
  datasummary_df: { name: 'datasummary_df', parent: 'modelsummary' },

  print: { name: 'print', parent: 'base' },

  render: { name: 'render', parent: 'rmarkdown' }
}

export const getFormatters = (functions: string[]) => {
  return functions.map(fn => formatters[fn as keyof typeof formatters]).filter(Boolean)
}
