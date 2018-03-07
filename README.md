# LibrarianXlsx
Created for the Facultad de Ciencias Exactas, Ingenieria y Agrimensura, 2017

## Introduction
LibrarianXlsx is a bookkeeping software, intended for small libraries. It was created on the premises of being useful for the Computer Science Department, with educative -and hopefully practical- purposes.

## Getting Started
This software was compiled with GHC v7.10.3, using xlsx-0.6.0

## "XlsxWrappers.hs" exposed functions
**readCell** :: FilePath -> XlsxSheet -> XlsxCell -> IO (Maybe CellValue)

*readCell reads the xlsx file, and the value from the corresponding sheet and cell. Before returning that value it calculates the length of the file, as to break lazyness. This helps with composing or concatenating this function with others.*

**writeCell** :: FilePath -> XlsxSheet -> XlsxCell -> CellValue -> IO ()

*writeCell reads the xlsx file, and changes the value from the corresponding sheet and cell to the value provided. Before returning it calculates the length of the file, as to break lazyness. This helps with composing or concatenating this function with others.*

**deleteCell** :: FilePath -> XlsxSheet -> XlsxCell -> IO ()

*deleteCell reads the xlsx file, and changes the value from the corresponding sheet and cell to Nothing. Before returning it calculates the length of the file, as to break lazyness. This helps with composing or concatenating this function with others.*

**appendCell** :: FilePath -> XlsxSheet -> XlsxCell -> CellValue -> IO ()

*appendCell does a readCell from the file, and concatenates text or sums numbers, depending on the cell and value type. Numbers can be concatenated to text, but will become text. Text cannot be concantenated to numbers.*

**createXlsxI** :: FilePath -> XlsxSheet -> Worksheet -> IO ()

*createXlsxI creates a new xlsx file, initialized with the provided sheet, in the specified directory.*

**createXlsxU** :: FilePath -> IO ()

*createXlsxU creates a new xlsx file, empty.*

**addSheet** :: FilePath -> XlsxSheet -> IO ()

*addSheet creates a new xlsx sheet, empty. Careful!! if the name already exists, it deletes all content.*

**renameSheet** :: FilePath -> XlsxSheet -> XlsxSheet -> IO ()

*renameSheet renames an existing xlsx sheet. Keeps contents intact.*

**deleteSheet** :: FilePath -> XlsxSheet -> IO ()

*deleteSheet deletes an existing xlsx sheet and all its contents.*

