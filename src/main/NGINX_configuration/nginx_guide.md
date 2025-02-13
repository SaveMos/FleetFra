La relazione tra max_fails/fail_timeout e proxy_next_upstream_timeout è la seguente:

proxy_next_upstream_timeout 1s:

Definisce il tempo massimo che Nginx aspetta per una risposta prima di considerare un server non responsivo e provare un altro server disponibile.
Questo si applica solo per una singola richiesta.
max_fails=2 fail_timeout=5s:

Se un server fallisce 2 volte consecutive in un intervallo di 5 secondi, Nginx lo considera temporaneamente non disponibile e smette di inviare richieste a quel server per un certo periodo.
Dopo il fail_timeout, il server viene testato nuovamente con una richiesta reale. Se risponde correttamente, viene reinserito nel pool.
Come Funzionano Insieme?
Primo errore:

Se un server non risponde entro 1 secondo, Nginx considera la richiesta fallita e passa al prossimo server disponibile nel pool (se c'è).
Questo è controllato da proxy_next_upstream_timeout 1s.
Secondo errore (entro 5 secondi):

Se un server fallisce ancora entro 5 secondi, raggiunge il limite definito da max_fails=2, e quindi viene temporaneamente rimosso dal pool.
Dopo fail_timeout=5s:

Il server viene testato automaticamente.
Se risponde correttamente, viene reintrodotto nel pool immediatamente senza bisogno di un riavvio.
Caso Pratico
Supponiamo di avere tre server e uno di essi inizia ad avere problemi:

Nginx invia una richiesta a 10.2.1.30, ma non risponde entro 1 secondo → Passa a 10.2.1.29.
Nginx invia un’altra richiesta a 10.2.1.30, ma fallisce di nuovo entro 5 secondi → Lo esclude temporaneamente.
Dopo 5 secondi, Nginx prova di nuovo 10.2.1.30 con una richiesta reale.
Se risponde correttamente, viene riattivato nel pool immediatamente.
Se fallisce di nuovo, rimane escluso per altri 5 secondi.