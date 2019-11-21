      ******************************************************************
      * Author: Grupo 03
      * Date: 23/11/2019
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Cadastro-Bancario.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           Special-names.
                   DECIMAL-POINT is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTES ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               FILE STATUS IS ARQ-STATUS
               RECORD KEY IS NumConta.
       DATA DIVISION.
       FILE SECTION.
           FD ARQ-CLIENTES VALUE OF FILE-ID IS "CLIENTES.DAT".
           01 DADOS-ARQ.
               02 CodBanco PIC 9(03).
               02 CodAgencia PIC 9(04).
               02 TipoConta PIC 9(01).
               02 NumConta PIC 9(11).
               02 NomeTitular PIC X(20).
               02 CPF PIC 9(11).
               02 CNPJ PIC 9(14).
               02 Saldo PIC 9(05)v99.
               02 Consulta PIC 9(14).

       WORKING-STORAGE SECTION.
           77 ARQ-STATUS PIC 9(02).
               88 STATUS-OK VALUE ZEROS.
               88 STATUS-NAO-EXISTE VALUE 23.
               88 STATUS-NAO-ENCONTRADO VALUE 35.
               
           01 DADOS.
               02 Opcao PIC X(01).
               02 CPF-E PIC 999.999.999.99 VALUE ZEROS.
               02 CNPJ-E PIC 99.999.999.9999.99 VALUE ZEROS.
               02 Saldo-E PIC ZZ9,99.

           01 MENSAGENS-DE-TELA.
               02 M-ENTER PIC X(30) VALUE "Tecle Enter para continuar.".
               02 M1 PIC X(50) VALUE "Codigo do banco: ".
               02 M2 PIC X(50) VALUE "Agencia (sem digito): ".
               02 M3 PIC X(50) VALUE "Tipos de Conta".
               02 M4 PIC X(50) VALUE "1 - Conta Corrente PF".
               02 M5 PIC X(50) VALUE "2 - Conta Poupanca PF".
               02 M6 PIC X(50) VALUE "3 - Conta Corrente PJ".
               02 M7 PIC X(50) VALUE "4 - Conta Poupanca PJ".
               02 M8 PIC X(50) VALUE "Numero da conta (com digito): ".
               02 M9 PIC X(50) VALUE "Nome/Razao Social: ".
               02 M10 PIC X(50) VALUE "CPF: ".
               02 M11 PIC X(50) VALUE "CNPJ: ".
               02 M12 PIC X(50) VALUE "Saldo Bancario: ".
               02 M13 PIC X(50) VALUE "Digite o CPF ou CNPJ: ".
               02 M14 PIC X(50) VALUE "Cadastro realizado!".
               02 ME1 PIC X(30) VALUE "Erro ao realizar o cadastro.".
               02 ME2 PIC X(30) VALUE "Cadastro nao encontrado.".
               02 ME3 PIC X(30) VALUE "Registro nao encontrado.".
               02 ME4 PIC X(30) VALUE "Erro ao reescrever dados.".
               02 ME5 PIC X(30) VALUE "Registro nao encontrado.".

           01 DATA-DO-SISTEMA.
               02 ANO PIC 9(02) VALUE ZEROS.
               02 MES PIC 9(02) VALUE ZEROS.
               02 DIA PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
           01 TELA01.
               02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11 PIC 9(02) USING ANO.
               02 LINE 02 COLUMN 28 VALUE
                   "*** CADASTRO BANCARIO ***".

       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
           MAIN-FUNC.

           INICIO.
               ACCEPT DATA-DO-SISTEMA FROM DATE.
               DISPLAY TELA01 AT 0101.
               MOVE ZEROS TO DADOS.
               MOVE ZEROS TO DADOS-ARQ.
               PERFORM ABRIR-ARQ.
               PERFORM MENU-INICIAL.
               
           ABRIR-ARQ.
               OPEN I-O ARQ-CLIENTES
               IF STATUS-NAO-ENCONTRADO THEN
                   OPEN OUTPUT ARQ-CLIENTES
                   CLOSE ARQ-CLIENTES
                   OPEN I-O ARQ-CLIENTES
               END-IF.

           MENU-INICIAL.
               DISPLAY "*** MENU DE OPCOES ***" AT 0528.
               DISPLAY " " AT 0628.
               DISPLAY "1 - Incluir" AT 0728.
               DISPLAY "2 - Consultar" AT 0828.
               DISPLAY "3 - Alterar" AT 0928.
               DISPLAY "4 - Excluir" AT 1028.
               DISPLAY "0 - Finalizar programa" AT 1128.
               DISPLAY " " AT 1228.
               DISPLAY "Opcao desejada: " AT 1328.
               ACCEPT Opcao AT 1344.

               EVALUATE Opcao
                   WHEN 1
                       PERFORM INCLUSAO-DADOS
                   WHEN 2
                       PERFORM CONSULTA-DADOS
                   WHEN 3
                       PERFORM ALTERA-DADOS
                   WHEN 4
                       PERFORM EXCLUI-DADOS
                   WHEN 0
                       PERFORM FIM-PROGRAMA.

               IF Opcao > 4 THEN
                   DISPLAY "Opcao invalida! Enter para voltar" AT 1428.
                   ACCEPT OMITTED.
                   DISPLAY SPACE ERASE EOS.
                   PERFORM INICIO.

           INCLUSAO-DADOS.
               DISPLAY M1 AT 1630.
               ACCEPT CodBanco AT 1648.

               DISPLAY M2 AT 1730.
               ACCEPT CodAgencia AT 1752.

               DISPLAY M3 AT 1830.
               DISPLAY M4 AT 1930.
               DISPLAY M5 AT 2030.
               DISPLAY M6 AT 2130.
               DISPLAY M7 AT 2230.
               DISPLAY "Digite o tipo: " AT 2330.
               ACCEPT TipoConta AT 2348.

               DISPLAY M8 AT 2430.
               ACCEPT NumConta AT 2460.

               DISPLAY M9 AT 2530.
               ACCEPT NomeTitular AT 2550.

               EVALUATE TipoConta
                   WHEN 1
                       DISPLAY M10 AT 2630
                       ACCEPT CPF AT 2650
                       MOVE CPF TO CPF-E
                   WHEN 2
                       DISPLAY M10 AT 2630
                       ACCEPT CPF AT 2650
                       MOVE CPF TO CPF-E
                   WHEN 3
                       DISPLAY M11 AT 2630
                       ACCEPT CNPJ AT 2650
                       MOVE CNPJ TO CNPJ-E
                   WHEN 4
                       DISPLAY M11 AT 2630
                       ACCEPT CNPJ AT 2650
                       MOVE CNPJ TO CNPJ-E.

               WRITE DADOS-ARQ
                   INVALID KEY
                       DISPLAY ME1 AT 2830
                       DISPLAY M-ENTER AT 2860
                   NOT INVALID KEY
                       DISPLAY M14 AT 2830
               END-WRITE.
               
               ACCEPT OMITTED.
               
               DISPLAY SPACE ERASE EOS.
               PERFORM INICIO.

           CONSULTA-DADOS.
               DISPLAY M8 AT 1630.
               ACCEPT NumConta AT 1660.
               
               READ ARQ-CLIENTES
                   INVALID KEY
                       DISPLAY ME2 AT 1730
                       DISPLAY M-ENTER AT 1830
                   NOT INVALID KEY
                       DISPLAY CodBanco AT 1830
                       DISPLAY CodAgencia AT 1930
                       DISPLAY TipoConta AT 2030
                       DISPLAY NumConta AT 2130
                       DISPLAY NomeTitular AT 2230
                       DISPLAY CPF AT 2330
                       DISPLAY CNPJ AT 2430
                       DISPLAY Saldo AT 2530
                       DISPLAY Consulta AT 2630
                       DISPLAY M-ENTER AT 2830
               END-READ.

               ACCEPT OMITTED.
               DISPLAY SPACE ERASE EOS.
               PERFORM LIMPAR-DADOS-ARQ.
               PERFORM INICIO.

           ALTERA-DADOS.
               DISPLAY M8 AT 1630.
               ACCEPT NumConta AT 1660.
               
               READ ARQ-CLIENTES
                   INVALID KEY
                       IF STATUS-NAO-EXISTE THEN
                           DISPLAY ME3 AT 1830
                           DISPLAY M-ENTER AT 1930
                       END-IF
                       PERFORM LIMPAR-DADOS-ARQ
                       GO TO MENU-INICIAL
               END-READ.
                   
               PERFORM RECEBER-DADOS-PARA-ATUALIZACAO.

               REWRITE DADOS-ARQ
                   INVALID KEY
                       DISPLAY ME4 AT 2830
                       DISPLAY M-ENTER AT 2860
                   NOT INVALID KEY
                       DISPLAY "Dados atualizados com sucesso." AT 2830
                       DISPLAY M-ENTER AT 2930
               END-REWRITE.
                   
               ACCEPT OMITTED.
               DISPLAY SPACE ERASE EOS.
               PERFORM LIMPAR-DADOS-ARQ.
               PERFORM INICIO.

           EXCLUI-DADOS.
               DISPLAY M8 AT 1630.
               ACCEPT NumConta AT 1660.

               DELETE ARQ-CLIENTES
                   INVALID KEY
                       DISPLAY ME5 AT 1830
                       DISPLAY M-ENTER AT 1860
                   NOT INVALID KEY
                       DISPLAY "Registro removido com sucesso." AT 1830
                       DISPLAY M-ENTER AT 1930
               END-DELETE.
               
               PERFORM LIMPAR-DADOS-ARQ.
               PERFORM INICIO.
           
           LIMPAR-DADOS-ARQ.
               MOVE ZEROS TO DADOS-ARQ.
               CLOSE ARQ-CLIENTES.
               OPEN I-O ARQ-CLIENTES.
           
           RECEBER-DADOS-PARA-ATUALIZACAO.
               DISPLAY M1 AT 1630.
               ACCEPT CodBanco AT 1648.

               DISPLAY M2 AT 1730.
               ACCEPT CodAgencia AT 1752.

               DISPLAY M3 AT 1830.
               DISPLAY M4 AT 1930.
               DISPLAY M5 AT 2030.
               DISPLAY M6 AT 2130.
               DISPLAY M7 AT 2230.
               DISPLAY "Digite o tipo: " AT 2330.
               ACCEPT TipoConta AT 2348.

               DISPLAY M9 AT 2530.
               ACCEPT NomeTitular AT 2550.

               EVALUATE TipoConta
                   WHEN 1
                       DISPLAY M10 AT 2630
                       ACCEPT CPF AT 2650
                       MOVE CPF TO CPF-E
                   WHEN 2
                       DISPLAY M10 AT 2630
                       ACCEPT CPF AT 2650
                       MOVE CPF TO CPF-E
                   WHEN 3
                       DISPLAY M11 AT 2630
                       ACCEPT CNPJ AT 2650
                       MOVE CNPJ TO CNPJ-E
                   WHEN 4
                       DISPLAY M11 AT 2630
                       ACCEPT CNPJ AT 2650
                       MOVE CNPJ TO CNPJ-E.

           FIM-PROGRAMA.
               CLOSE ARQ-CLIENTES.
               STOP RUN.


       END PROGRAM Cadastro-Bancario.
