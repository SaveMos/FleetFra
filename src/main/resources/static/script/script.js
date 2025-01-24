const userGrid = Array(11).fill(0).map(() => Array(11).fill(0));
const opponentGrid = Array(11).fill(0).map(() => Array(11).fill(0));
let isVertical = false
const shipArray = [
    {
        name: "single",
        size: 1,
        index: 0
    },
    {
        name: "single",
        size: 1,
        index: 1
    },
    {
        name: "single",
        size: 1,
        index: 2
    },
    {
        name: "single",
        size: 1,
        index: 3
    },
    {
        name: "double",
        size: 2,
        index: 4
    },
    {
        name: "double",
        size: 2,
        index: 5
    },
    {
        name: "double",
        size: 2,
        index: 6
    },
    {
        name: "triple",
        size: 3,
        index: 7
    },
    {
        name: "triple",
        size: 3,
        index: 8
    },
    {
        name: "quadruple",
        size: 4,
        index: 9
    }
]

function createGrid(gridId) {
    const grid = document.getElementById(gridId);

    // Etichette orizzontali (A-J)
    const letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'];

    // Aggiungi l'intestazione orizzontale (le lettere A-J)
    for (let i = 0; i <= 10; i++) {
        for (let j = 0; j <= 10; j++) {
            const cell = document.createElement('div');
            if (i === 0 && j === 0) {
                cell.classList.add('cell', 'cell-header');
            } else if (i === 0) {
                cell.classList.add('cell', 'cell-header');
                cell.textContent = letters[j - 1];
            } else if (j === 0) {
                cell.classList.add('cell', 'cell-header');
                cell.textContent = i.toString();
            } else {
                cell.classList.add('cell');
            }
            grid.appendChild(cell);
        }
    }
}

// Dimensioni delle celle della griglia
const cellSize = 30;

// Posizionamento iniziale dinamico degli elementi nella fleet
const fleetItems = document.querySelectorAll('.fleet-item');
fleetItems.forEach((item, index) => {
    //const row = Math.floor(index / 6); // 5 elementi per riga
    //const col = index % 6;
    const row = 0; // Tutti sulla stessa riga
    const col = index - 2; // Posiziona in ordine orizzontale, (-2) utilizzato per centrare
    item.style.transform = `translate(${col * (cellSize)}px, ${row * (cellSize)}px)`;
    item.setAttribute('data-x', col * (cellSize));
    item.setAttribute('data-y', row * (cellSize));
});








// Funzione per ruotare gli elementi della flotta
function rotateFleet() {
    const fleetItems = document.querySelectorAll('.fleet-item');
    isVertical = !isVertical; // Alterna lo stato

    fleetItems.forEach(item => {
        const baseClass = item.className.split(' ')[1]; // Ottieni il tipo di nave (es: single, double)
        item.className = `fleet-item ${baseClass}`; // Resetta le classi
        if (isVertical) {
            item.classList.add(`${baseClass}-vertical`); // Aggiunge la classe verticale
        }
    });
}

// Aggiungi un event listener al pulsante di rotazione
document.getElementById('rotateButton').addEventListener('click', rotateFleet);


// Crea le griglie
window.onload = function () {
    createGrid('grid1');
    createGrid('grid2');
};
