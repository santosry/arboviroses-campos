# Script auxiliar para escrever o arquivo mosquito.svg
svg_lines <- c(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">',
  '  <ellipse cx="32" cy="36" rx="6" ry="12" fill="#3B5998" stroke="#1B3A6B" stroke-width="1.5"/>',
  '  <circle cx="32" cy="22" r="4.5" fill="#3B5998" stroke="#1B3A6B" stroke-width="1"/>',
  '  <line x1="32" y1="18" x2="32" y2="10" stroke="#1B3A6B" stroke-width="2" stroke-linecap="round"/>',
  '  <ellipse cx="18" cy="30" rx="10" ry="4" fill="#A8C4E8" stroke="#7BA3D1" stroke-width="0.8" transform="rotate(-30 18 30)" opacity="0.6"/>',
  '  <ellipse cx="46" cy="30" rx="10" ry="4" fill="#A8C4E8" stroke="#7BA3D1" stroke-width="0.8" transform="rotate(30 46 30)" opacity="0.6"/>',
  '  <line x1="26" y1="34" x2="12" y2="28" stroke="#1B3A6B" stroke-width="1.2" stroke-linecap="round"/>',
  '  <line x1="26" y1="38" x2="10" y2="42" stroke="#1B3A6B" stroke-width="1.2" stroke-linecap="round"/>',
  '  <line x1="26" y1="42" x2="14" y2="52" stroke="#1B3A6B" stroke-width="1.2" stroke-linecap="round"/>',
  '  <line x1="38" y1="34" x2="52" y2="28" stroke="#1B3A6B" stroke-width="1.2" stroke-linecap="round"/>',
  '  <line x1="38" y1="38" x2="54" y2="42" stroke="#1B3A6B" stroke-width="1.2" stroke-linecap="round"/>',
  '  <line x1="38" y1="42" x2="50" y2="52" stroke="#1B3A6B" stroke-width="1.2" stroke-linecap="round"/>',
  '  <line x1="30" y1="20" x2="24" y2="14" stroke="#7BA3D1" stroke-width="1" stroke-linecap="round"/>',
  '  <line x1="34" y1="20" x2="40" y2="14" stroke="#7BA3D1" stroke-width="1" stroke-linecap="round"/>',
  '  <circle cx="30" cy="21" r="1.5" fill="#E74C3C"/>',
  '  <circle cx="34" cy="21" r="1.5" fill="#E74C3C"/>',
  '</svg>'
)
writeLines(svg_lines, "www/mosquito.svg")
cat("Arquivo escrito com sucesso:", file.exists("www/mosquito.svg"), "\n")
cat("Tamanho:", file.info("www/mosquito.svg")$size, "bytes\n")
cat("Arquivos em www:", paste(list.files("www"), collapse = ", "), "\n")
