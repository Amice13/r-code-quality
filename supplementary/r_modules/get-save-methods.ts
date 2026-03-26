const writeFunctions = {
  cat: { name: 'cat', parent: 'base' },
  saveRDS: { name: 'saveRDS', parent: 'base' },
  save: { name: 'save', parent: 'base' },
  sink: { name: 'sink', parent: 'base' },
  'write.csv': { name: 'write.csv', parent: 'base' },
  'write.table': { name: 'write.table', parent: 'base' },
  writeLines: { name: 'writeLines', parent: 'base' },

  write_json: { name: 'write_json', parent: 'jsonlite' },

  dbWriteTable: { name: 'dbWriteTable', parent: 'DBI' },

  gtsave: { name: 'gtsave', parent: 'gt' },

  quick_latex: { name: 'quick_latex', parent: 'huxtable' },

  pdflatex: { name: 'pdflatex', parent: 'tinytex' },

  ggsave: { name: 'ggsave', parent: 'ggplot2' },

  png: { name: 'png', parent: 'base' },
  pdf: { name: 'pdf', parent: 'base' },
  jpeg: { name: 'jpeg', parent: 'base' },
  tiff: { name: 'tiff', parent: 'base' },
  bmp: { name: 'bmp', parent: 'base' },
  svg: { name: 'svg', parent: 'base' },
  postscript: { name: 'postscript', parent: 'base' },
  'win.metafile': { name: 'win.metafile', parent: 'base' },

  agg_png: { name: 'agg_png', parent: 'ragg' },
  agg_jpeg: { name: 'agg_jpeg', parent: 'ragg' },
  agg_tiff: { name: 'agg_tiff', parent: 'ragg' },
  agg_capture: { name: 'agg_capture', parent: 'ragg' },

  Cairo: { name: 'Cairo', parent: 'Cairo' },
  CairoX11: { name: 'CairoX11', parent: 'Cairo' },
  CairoPNG: { name: 'CairoPNG', parent: 'Cairo' },
  CairoJPEG: { name: 'CairoJPEG', parent: 'Cairo' },
  CairoTIFF: { name: 'CairoTIFF', parent: 'Cairo' },
  CairoPDF: { name: 'CairoPDF', parent: 'Cairo' },
  CairoSVG: { name: 'CairoSVG', parent: 'Cairo' },
  CairoWin: { name: 'CairoWin', parent: 'Cairo' },
  CairoPS: { name: 'CairoPS', parent: 'Cairo' },
  'Cairo.capture': { name: 'Cairo.capture', parent: 'Cairo' },
  'Cairo.snapshot': { name: 'Cairo.snapshot', parent: 'Cairo' },

  'trellis.device': { name: 'trellis.device', parent: 'lattice' }
}


export const getSaveMethods = (functions: string[]) => {
  return functions.map(fn => writeFunctions[fn as keyof typeof writeFunctions]).filter(Boolean)
}
