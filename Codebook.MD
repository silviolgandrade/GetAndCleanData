## Codebook for Project

Codebook.MD - this file.

run_analysis.R - the script file, with detailed steps inside

Intend as the sources files are in a folder called "UCI HAR Dataset", as in the downloaded file.

When run the script, there are two output files:

Tabela1.txt - the tidy dataset with all values

Tabela2.txt - the tidy dataset with the average values of each variable for each activity and subject.

Into the script .R file there are information for each step in the processing.


Variables used in this script:

DadosTeste - toc of test/X_test.txt

DadosTreino - toc of train/X_train.txt

NomesTeste - toc of test/y_test.txt

NomesTreino - toc of train/y_train.txt

SujeitoTeste - toc of test/subject_test.txt

SujeitoTreino - toc of test/subject_train.txt

ListaRecursos - toc of features.txt

Atividades - toc of activity_labels.txt

Dados - Combined data set of the variables X_test and X_train.

Nomes - Combined data set of the variables Y_test and Y_train.

Sujeitos - Combined data set of the variables subject_test and subject_train.

Recursos - Titles for data columns derived from ListRecursos

Colunas - vector of names values from Recursos to use in tidy data set

Tabela1 - First output file, tidy dataset subsetted, with all content

UnicosSujeitos - unique subjects from Sujeitos

NumeroSujeitos - number of unique subjects from Sujeitos

NumeroAtividades - number of Atividades

NumeroColunas - cols in Tabela1

Tabela2 - Second output file, tiny data set with average of each variable for each activity and subject
