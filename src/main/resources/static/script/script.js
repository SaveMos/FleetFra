function createGrid(gridId) {
    const grid = document.getElementById(gridId);

    // Etichette orizzontali (A-J)
    const letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'];

    // Aggiungi l'intestazione orizzontale (le lettere A-J)
    for (let i = 0; i <= 10; i++) {  // 11 per includere l'etichetta vuota in alto a sinistra
        for (let j = 0; j <= 10; j++) {
            if (i === 0 && j === 0) {
                // Angolo in alto a sinistra (vuoto)
                const headerCell = document.createElement('div');
                headerCell.classList.add('cell');
                grid.appendChild(headerCell);
            } else if (i === 0) {
                // Etichetta orizzontale (A-J)
                const headerCell = document.createElement('div');
                headerCell.classList.add('cell', 'cell-header');
                headerCell.textContent = letters[j - 1]; // -1 per partire da A
                grid.appendChild(headerCell);
            } else if (j === 0) {
                // Etichetta verticale (1-10)
                const headerCell = document.createElement('div');
                headerCell.classList.add('cell', 'cell-header');
                headerCell.textContent = i.toString();
                grid.appendChild(headerCell);
            } else {
                // Celle della griglia
                const cell = document.createElement('div');
                cell.classList.add('cell');
                grid.appendChild(cell);
            }
        }
    }
}

// Posizionamento iniziale dinamico degli item
const fleetContainer = document.getElementById("fleet");
const fleetItems = document.querySelectorAll(".fleet-item");

fleetItems.forEach((item, index) => {
    if (index < 8) {
        // Posiziona i primi 8 elementi sulla prima riga
        item.style.left = `${index * 70}px`;
        item.style.top = "20px";
    } else {
        // Posiziona gli ultimi 2 elementi sulla prima riga
        item.style.left = `${(index - 8) * 90}px`;
        item.style.top = "90px";
    }
});

// Rendi ogni elemento della flotta trascinabile
fleetItems.forEach((item) => {
    dragElement(item);
});

function dragElement(elmnt) {
    var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;

    elmnt.onmousedown = dragMouseDown;

    function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        // Ottieni la posizione iniziale del mouse
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        // Calcola la nuova posizione
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // Imposta la nuova posizione dell'elemento
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        // Ferma il movimento quando il pulsante del mouse viene rilasciato
        document.onmouseup = null;
        document.onmousemove = null;
    }
}

window.onload = function() {
    createGrid('grid1');
    createGrid('grid2');
    /*
    // Rendi i div della fleet trascinabili
    const fleetItems = document.querySelectorAll('.fleet-item');
    fleetItems.forEach(item => {
      dragElement(item);
    }); */
};
