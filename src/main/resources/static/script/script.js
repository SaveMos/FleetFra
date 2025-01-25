// Inizializzazione della griglia dell'utente come array bidimensionale 11x11, con tutte le celle inizialmente vuote (valore 0)
const userGrid = Array(11).fill(0).map(() => Array(11).fill(0));


// Variabile per tracciare lo stato di orientamento della flotta (orizzontale o verticale)
let isHorizontal = true;

// Definizione delle navi disponibili con nome, dimensione e indice univoco
const shipArray = [
    { name: "single", size: 1, index: 0 },
    { name: "single", size: 1, index: 1 },
    { name: "single", size: 1, index: 2 },
    { name: "single", size: 1, index: 3 },
    { name: "double", size: 2, index: 4 },
    { name: "double", size: 2, index: 5 },
    { name: "double", size: 2, index: 6 },
    { name: "triple", size: 3, index: 7 },
    { name: "triple", size: 3, index: 8 },
    { name: "quadruple", size: 4, index: 9 }
];

// Rotazione della flotta: il bottone consente di alternare tra orientamento orizzontale e verticale
const rotateButton = document.querySelector("#rotateButton");
rotateButton.addEventListener("click", rotateShips);

// Funzione per alternare l'orientamento della flotta
function rotateShips() {
    const fleetItems = document.querySelectorAll('.fleet_item');
    fleetItems.forEach((item) => {
        const classes = item.classList;
        // Modifica le classi CSS per riflettere il cambiamento di orientamento
        if (isHorizontal) {
            classes.add(`${item.classList[1]}-vertical`);
            classes.remove(`${item.classList[1]}`);
        } else {
            classes.add(`${item.classList[1].replace('-vertical', '')}`);
            classes.remove(`${item.classList[1]}`);
        }
    });
    isHorizontal = !isHorizontal; // Inverte lo stato di orientamento
}

// Funzione per creare la griglia di gioco
function createGrid(gridId) {
    const grid = document.getElementById(gridId); // Seleziona il contenitore della griglia
    const letters = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]; // Intestazioni delle colonne

    for (let i = 0; i <= 10; i++) {
        for (let j = 0; j <= 10; j++) {
            const cell = document.createElement("div");
            cell.classList.add("cell"); // Aggiunge la classe CSS base per le celle

            if (i === 0 && j === 0) {
                cell.classList.add("cell-header"); // Angolo in alto a sinistra vuoto
            } else if (i === 0) {
                cell.textContent = letters[j - 1]; // Intestazioni delle colonne
                cell.classList.add("cell-header");
            } else if (j === 0) {
                cell.textContent = i.toString(); // Intestazioni delle righe
                cell.classList.add("cell-header");
            } else {
                // ID univoco della cella basato su griglia, riga e colonna
                cell.id = `${gridId === "grid1" ? "user" : "opponent"},${i},${j}`;
            }
            grid.appendChild(cell); // Aggiunge la cella alla griglia
        }
    }
}

// Funzione per gestire il mouseover, evidenziando le celle idonee al posizionamento della nave
function handleDivMouseOver(e) {
    const [_, row, col] = e.target.id.split(',').map(Number); // Estrae riga e colonna dalla cella
    let isValid = true;

    for (let i = 0; i < e.target.shipSize; i++) {
        const targetRow = isHorizontal ? row : row + i; // Calcola la posizione delle celle target
        const targetCol = isHorizontal ? col + i : col;

        // Verifica se le celle sono valide per il posizionamento (>0) così non è valido in caso di 0 oppure -1
        if (targetRow > 10 || targetCol > 10 || userGrid[targetRow][targetCol] !== 0) {
            console.log("nave non valida");
            isValid = false;
            break;
        }
    }

    if (isValid) {
        console.log("nave valida");
        // Evidenziazione delle celle valide
        for (let i = 0; i < e.target.shipSize; i++) {
            const targetRow = isHorizontal ? row : row + i;
            const targetCol = isHorizontal ? col + i : col;
            document.getElementById(`user,${targetRow},${targetCol}`).classList.add("eligible");
        }
    }
}

// Funzione per gestire il mouseout, rimuovendo l'evidenziazione
function handleDivMouseOut(e) {
    const [_, row, col] = e.target.id.split(',').map(Number);

    for (let i = 0; i < e.target.shipSize; i++) {
        const targetRow = isHorizontal ? row : row + i;
        const targetCol = isHorizontal ? col + i : col;
        if (targetRow <= 10 && targetCol <= 10) {
            document.getElementById(`user,${targetRow},${targetCol}`).classList.remove("eligible");
        }
    }
}

// Funzione per gestire il click sulle celle e posizionare la nave
function handleDivClick(e) {
    if (e.target.classList.contains("eligible")) {
        const [_, row, col] = e.target.id.split(',').map(Number);

        const ship = {
            name: shipArray[e.target.ship.id].name, // Nome della nave
            row,
            column: col,
            size: e.target.shipSize,
            direction: isHorizontal ? 1 : 0, // Orientamento: 1 per orizzontale, 0 per verticale
        };

        placeShip(ship, "user"); // Posiziona la nave nella griglia
        //rimuoviamo la nave dai listener in modo che non possa essere inserita una volta reinserita
        for (let row = 1; row <= 10; row ++) {
            for (let col = 1; col <= 10; col ++) {
                let trash = document.getElementById("user,"+row+","+col);
                trash.removeEventListener("mouseover", handleDivMouseOver);
                trash.removeEventListener("mouseout", handleDivMouseOut);
                trash.removeEventListener("click", handleDivClick);
            }
        }

        const fleetItem = document.getElementById(e.target.ship.id); // Rimuove la nave dalla lista
        fleetItem.remove();
    }
}

function placeShip(ship, boardName) {
    const deltas = [-1, 0, 1];

    for (let i = 0; i < ship.size; i++) {
        const targetRow = ship.direction === 1 ? ship.row : ship.row + i;
        const targetCol = ship.direction === 1 ? ship.column + i : ship.column;

        userGrid[targetRow][targetCol] = 1; // Segna la cella come occupata nella griglia
        const cell = document.getElementById(`${boardName},${targetRow},${targetCol}`);
        cell.classList.remove("eligible"); // Rimuove eligible e assegna taken
        cell.classList.add("taken", ship.name); // Aggiunge classi CSS per indicare la nave
    }

    // Assegna la classe "unavailable" alle celle confinanti
    for (let i = 0; i < ship.size; i++) {
        const row = ship.direction === 1 ? ship.row : ship.row + i;
        const col = ship.direction === 1 ? ship.column + i : ship.column;

        deltas.forEach((dx) => {
            deltas.forEach((dy) => {
                const adjacentRow = row + dx;
                const adjacentCol = col + dy;

                if (
                    adjacentRow > 0 &&
                    adjacentRow <= 10 &&
                    adjacentCol > 0 &&
                    adjacentCol <= 10 &&
                    userGrid[adjacentRow][adjacentCol] === 0
                ) {
                    userGrid[adjacentRow][adjacentCol] = -1; // Segna la cella come non disponibile
                    const adjacentCell = document.getElementById(`${boardName},${adjacentRow},${adjacentCol}`);
                    if (adjacentCell) {
                        adjacentCell.classList.add("unavailable");
                    }
                }
            });
        });
    }
}

// Inizializzazione della pagina
window.onload = function () {
    createGrid("grid1"); // Crea la griglia dell'utente
    createGrid("grid2"); // Crea la griglia dell'avversario

    const fleetItems = document.querySelectorAll('.fleet_item');
    fleetItems.forEach((item) => {
        // Aggiunge gli eventi di mouseover, mouseout e click per ogni cella
        item.addEventListener("click", () => {
            document.querySelectorAll(".cell").forEach((cell) => {
                // Verifica che la cella sia nella grid1 (controllo tramite id)
                if (cell.closest('#grid1')) {
                    cell.ship = item;
                    cell.shipSize = shipArray[item.id].size;
                    cell.addEventListener("mouseover", handleDivMouseOver);
                    cell.addEventListener("mouseout", handleDivMouseOut);
                    cell.addEventListener("click", handleDivClick);
                }
            });
        });
    });
};
